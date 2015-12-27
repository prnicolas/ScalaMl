/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99.1
 */
package org.scalaml.supervised.regression.logistic

import scala.util.{Try, Success, Failure, Random}
import scala.collection._
import org.apache.log4j.Logger
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core
import org.scalaml.util.{LoggingUtils, DisplayUtils, MathUtils}
import LoggingUtils._, MathUtils._
import org.scalaml.stats.XTSeries



	/**
	 * Define the model for the binomial logistic regression: it consists of the
	 * array of weights (one weight per dimension) and the intercept value
	 * @constructor Create a model for the binomial logistic regression
	 * @param weights array of weights
	 * 
	 * @author Patrick Nicolas
	 * @since 0.99
	 * @version 0.99.1
	 * @see Scala for Machine Learning Chapter 1 "Getting Started" Wrting an application
	 * @see org.scalaml.supervised.regression.RegressionModel
	 * @note The binomial logistic regression computes the '''intercept''' (or weight for observations
	 * of zero values) independently from the optimization of the weights. Multi-nomial linear
	 * and logistic regression include the intercept '''w0''' into the optimization.
	 */

case class LogBinRegressionModel(weights: DblArray) {
  override def toString: String =  s"weights: ${weights.mkString(",")}"
}


		/**
		 * Logistic regression for a binary (2-class) classifier. The number of weights (model) is 
		 * defined by the number of variable in each observation + 1.
		 * The training (extraction of the weights) is computed as part of the instantiation of the 
		 * class so the model is either complete or undefined so classification is never done on 
		 * incomplete (or poorly trained) model (computation error, maximum number of iterations 
		 * exceeded).
     * {{{{ 
     * For a vector of pair of observations and labels (x, y)
		 * Likelihood (conditional probability)
		 *    1/(1 + exp(-(w(0) + w(1).x(1) + w(2).x(2)))
		 * Batch descent gradient formula for a learning rate eta
		 *    weights <- weights - eta.(predicted - y).x
     * }}}
		 * @constructor Create a simple logistic regression for binary classification 
		 * @param expected expected values or labels used to train a model
		 * @param maxIters Maximum number of iterations used during training
		 * @param eta Slope used in the computation of the gradient
		 * @param eps Convergence criteria used to exit the training loop.
		 * @throws IllegalArgumentException if labels are undefined, or the values of maxIters, eta 
		 * or eps are out of range
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98 January 11, 2014 
     * @version 0.99.1
		 * @see Scala for Machine Learning Chapter 1 "Getting started" Let's kick the tires / Writing 
		 * a simple workflow
     * @see org.scalaml.supervised.regression.logistic.LogisticRegression
     * @see Design template for supervised learning algorithm Appendice / Design template for 
     * classifiers
     * @note This is a simplified version of the logistic regression is presented to illustrate 
     * the application. of a machine learning algorithm in chapter 1. A formal implementation of 
     * the logistic regression (chap 6)  is defined in class LogisticRegression
     * @see org.scalaml.supervised.regression.logistics.LogisticRegression
		 */
@throws(classOf[IllegalArgumentException])
final class LogBinRegression(
		obsSet: Vector[DblArray],
		expected: Vector[Double],
		maxIters: Int, 
		val eta: Double, 
		eps: Double) extends Monitor[Double] {
  
	import LogBinRegression._

	check(obsSet, expected, maxIters, eta, eps)
	
	protected val logger = Logger.getLogger("LogBinRegression")

		/**
		 * Definition of the modelfor the binomial logistic regression
		 */
	val model: LogBinRegressionModel  = train
  
		/**
		 * classification of a two dimension data (xy) using a binomial logistic regression.
		 * @param obs a new observation
		 * @return Try (class, likelihood) for the logistic regression is the training was completed
		 * , None otherwise
		 */
	def classify(obs: DblArray): Try[(Int, Double)] = Try {
		val linear = dot(obs, model.weights) 
		val prediction = sigmoid(linear)
		(if(linear > 0.0) 1 else 0, prediction)
	 }
	
	
	def classify(obs: DblPair): Try[(Int, Double)] = classify(Array[Double](obs._1, obs._2))
	
		/**
		 * Implements the training algorithm for the logistic regression. The model (weights)
		 * is not initialized if the training loop does not converge before the maximum 
		 * number of iterations is reached. The training method relies on the batch gradient descent
		 * which is implemented as a tail recursion
		 */
	private def train: LogBinRegressionModel = {
		val scale = 0.5/obsSet.size

		@scala.annotation.tailrec
		def gradientDescent(
				obsAndLbl: LabelObs, 
				cost: Double, 
				nIters: Int,
				weights: DblArray): DblArray = {
	
				// if the maximum number of iterations is reached
			if(nIters >= maxIters)
				throw new IllegalStateException(s"LogBinRegression.train failed after $nIters iterations")

				// Shuffle the vector of (observation, label) pairs for the next iteration
			val shuffled = shuffle(obsAndLbl)
		
					// Traverses the (observation, label) pairs set, to compute the prediced value
					// using the logistic function (sigmoid) and compare to the labeled data.	
			val errorGrad: (DblVector, Vector[DblArray]) = shuffled.map{ case (x, y) =>
				val inner = dot(x, weights)
					// Difference between the predicted and labeled data
				val error = sigmoid(inner) - y

				(error, x.map( _ * error))
			}.unzip
			
	
				// Compute the new cost as the sum of square value of the error for each point
			val newCost = errorGrad._1.aggregate(0.0)((s, c) => s + c*c, _ + _)*scale
			
				// Monitor counters update
			count(COUNT_ERR, newCost)
			weights.zipWithIndex.foreach{ case(w, n) => count(s"w$n", w) }
		  	
				// compute the incremental relative error
				// if the algorithm converges, returns the final weights
			if( Math.abs( cost/newCost - 1.0) < eps) 
				weights
				
				// otherwise launch a new iteration
			else {
				val derivatives = Vector[Double](1.0) ++ errorGrad._2.transpose.map(_.sum)

					// Apply the gradient descent update formula to compute new weights
				val newWeights = weights.zip(derivatives).map { case (w, df) => w - eta*df }

					// update the weights
				newWeights.copyToArray(weights)
		  		
					// recurse to the next data point.
				gradientDescent(shuffled, newCost, nIters+1, newWeights)
			}
		}
		
			// The weights are initialized as random values over [min labels, max labels]

		val initialWeights = Array.fill(obsSet.head.length+1)( Random.nextDouble() )
			
			// Apply the gradient descent.
		val weights = gradientDescent( obsSet.zip(expected), 0.0, 0, initialWeights)
		new LogBinRegressionModel(weights)
	}
	

	 /*
	  * Computation of the intercept independently from the optimization of
	  * the weights of the logistic regression
	  */
	private def intercept(weights: DblArray): Double = {
		val zeroObs = obsSet.filter(!_.exists( _ > 0.01))
		if( zeroObs.nonEmpty )
			zeroObs.aggregate(0.0)((s,z) => s + dot(z, weights), _ + _ )/zeroObs.size
		else 
		  0.0
	}

	
	private def learningRate(nIter: Int): Double = eta/(1 + eta*nIter/obsSet.size)
}


		/**
		 * Companion object for the simple logistic regression LogBinRegression. This singleton
		 * is used to define some constants and validate the class parameters.
		 * @author Patrick Nicolas
		 * @since 0.98 January 11, 2014 
     * @version 0.99.1
     * @note Add shuffling capabilities to the batch gradient descent algorithm
		 */
object LogBinRegression {
	final val HYPERPLANE = 0.5
	private val MAX_NUM_ITERS = 8192
	private val ETA_LIMITS = (1e-7, 1e-1)
	private val EPS_LIMITS = (1e-30, 0.01)
	
	final val SPAN = 6
	final val COUNT_ERR = "Error"
	  
	type LabelObs = Vector[(DblArray, Double)]
	
	
		/**
		 * Computes the dot product of observations and weights by adding the
		 * bias element or input to the array of observations
		 * @param obs Array of observations (dimension of the model)
		 * @param weights Array or (number of features + 1) weights
		 * @return w0 + w1.x1 + .. + wn.xn
		 * @throws IllegalArgumentException if obs.size != weights.size -1
		 */
	@throws(classOf[IllegalArgumentException])
	final def dot(obs: DblArray, weights: DblArray): Double = {
		require(obs.length + 1 == weights.length,
			s"LogBinRegression.dot found obs.length ${obs.length} +1 != weights.length ${weights.length}")

		weights.zip(Array[Double](1.0) ++ obs.view)
			.aggregate(0.0)((s,x) => s + x._1*x._2, _ + _)
	}
	
			/**
			 * Static method to shuffle the order of observations and labels between iterations
			 * the gradient descent algorithm. The method is implemented as a tail recursion.
			 * The shuffle is accomplish by partitioning the input data set in segments of random size
			 * and reverse the order of each other segment.
			 * @param labelObs input vector of pairs (multi-variable observation, label)
			 * @return vector of pairs (observation, label) which order has been shuffled
			 * @throws IllegalArgumentException if the labeled observations are undefined.
			 */
	@throws(classOf[IllegalArgumentException])
	def shuffle(labelObs: LabelObs): LabelObs = {
		require( labelObs.nonEmpty,
				"LogBinRegression.shuffle Cannot proceed with undefined labeled observations")
		
		import scala.util.Random 
		val maxChunkSize = Random.nextInt(SPAN)+2
	  
		val sz = labelObs.size
	  
		@scala.annotation.tailrec
		def shuffle(indices: mutable.ArrayBuffer[Int], count: Int, start: Int): Array[Int] = {
			val end = start + Random.nextInt(maxChunkSize)
			val isOdd = (count & 0x01) != 0x01

			if(end >= sz)
				indices.toArray ++ slice(isOdd, start, sz)
			else 
				shuffle(indices ++ slice(isOdd, start, end), count+1, end)
		}
	  
		def slice(isOdd: Boolean, start: Int, end: Int): Array[Int] = {
			val r = Range(start, end).toArray
			if (isOdd) r else r.reverse
		}
	  
		shuffle(new mutable.ArrayBuffer[Int], 0, 0).map(labelObs( _ ) ).toVector
	}	
	
	
	private def check(
			obsSet: Vector[DblArray], 
			expected: Vector[Double],
			maxIters: Int, 
			eta: Double, 
			eps: Double): Unit =  {
	  
		require(maxIters > 10 && maxIters < MAX_NUM_ITERS, 
				s"LogBinRegression found max iterations = $maxIters required 10 < .. < $MAX_NUM_ITERS")
		require(eta > ETA_LIMITS._1 && eta < ETA_LIMITS._2, 
				s"LogBinRegression found eta = $eta requires ${ETA_LIMITS._1} < . < ${ETA_LIMITS._2}")
		require(eps > EPS_LIMITS._1 && eps < EPS_LIMITS._2, 
				s"LogBinRegression  found $eps required ${EPS_LIMITS._1} < . < ${EPS_LIMITS._2}")
		require( obsSet.size == expected.size, 
				s"LogBinRegression found ${obsSet.size} observations != ${expected.size} labels, require ==")
	}
}


// ----------------------  EOF ------------------------------------
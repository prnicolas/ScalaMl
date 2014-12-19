/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96d
 */
package org.scalaml.supervised.regression.logistic

import org.apache.log4j.Logger

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.util.DisplayUtils



		/**
		 * <p>Logistic regression for a binary (2-class) classifier. The number of weights (model) is defined
		 * by the number of classifiers + 1. .<br>
		 * The training (extraction of the weights) is computed as part of the instantiation of the class so 
		 * the model is either complete or undefined so classification is never done on incomplete (or
		 * poorly trained) model (computation error, maximum number of iterations exceeded).<br>
		 * For simplicity, this first and simple logistic regression assumes a feature of two dimension
		 * (x, y) and consequently a vector of DIM = 3 weights.
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;
		 * ,Arial,Helvetica,sans-serif;">
		 * The likelihood (conditional probability is computed as 1/(1 + exp(-(w(0) + w(1).x(1) + w(2).x(2)))</span></pre></p>
		 * @constructor Create a logistic regression for binary classification 
		 * @param labels Data used to train a model
		 * @param maxIters Maximum number of iterations used during training
		 * @param eta Slope used in the computation of the gradient
		 * @param eps Convergence criteria used to exit the training loop.
		 * @throws IllegalArgumentException if labels are undefined, or the values of maxIters, eta 
		 * or eps are out of range
		 * 
		 * @author Patrick Nicolas
		 * @since January 11, 2014
		 * @note Scala for Machine Learning Chapter 1 Getting started / Let's kick the tires / Writing 
		 * a simple workflow
		 */
final class LogBinRegression(labels: DVector[(XY, Double)], maxIters: Int, eta: Double, eps: Double) {
	import LogBinRegression._
	
		// The weights define the model for this simple logistic regression
	private[this] val weights = train
	private val logger = Logger.getLogger("LogBinRegression")

		/**
		 * <p>classification of a two dimension data (xy) using a binary logistic regression.</p>
		 * @param xy (x,y) data point
		 * @return Option (class, likelihood) for the logistic regression is the training was completed
		 * , None otherwise
		 */
	def classify(xy: XY): Option[(Boolean, Double)] = weights.map(w => {
						// compute the logit of w0 + w1.x1 + w2.x2 
			val likelihood =  sigmoid(w(0) + xy._1*w(1) + xy._2*w(2))
			(likelihood > HYPERPLANE, likelihood)
	})
	
		/**
		 * Implements the training algorithm for the logistic regression. The model (weights)
		 * is not initialized if the training loop does not converge before the maximum 
		 * number of iterations is reached.
		 */
	private[this] def train: Option[DblVector] = {
		import scala.util.Random 
		
			// Random initializaion of the weights
		val w = Array.tabulate(DIM)( _ => Random.nextDouble-1.0) 
    
			// Iterates through the computation of the predicted value z
			// and the derivative dw
		(0 until maxIters).find( _ => {
	
				// Compute the sigmoid for the dot product then
				// update the gradient
			val deltaW = labels.foldLeft(Array.fill(DIM)(0.0)) ((dw, y) => {  
				val z = sigmoid(w(0) + w(1)*y._1._1 +  w(2)*y._1._2)
				dw.map(dx => dx + (y._2 - z)*(y._1._1 + y._1._2))
			})

				// Applies the descent gradient formula.
			val nextW = Array.fill(DIM)(0.0)
							 .zipWithIndex.map( nw => w(nw._2) + eta*deltaW(nw._2))
			val diff = Math.abs(nextW.sum - w.sum)
	    	
			nextW.copyToArray(w)
			diff < eps	// Exit condition
		}).map(_ => w)  // map to the vector of weights
	}
	
	private[this] def sigmoid(x: Double): Double = 1.0/(1.0 + Math.exp(-x))

}


		/**
		 * Companion object for the simple logistic regression LogBinRegression. This singleton
		 * is used to define some constants and validate the class parameters.
		 */
object LogBinRegression {
	final val DIM = 3
	final val HYPERPLANE = 0.5
	private val MAX_NUM_ITERS = 1024
	private val ETA_LIMITS = (1e-7, 1e-1)
	private val EPS_LIMITS = (1e-10, 0.25)
	
		
	private def check(labels: DVector[(XY, Double)], maxIters: Int, eta: Double, eps: Double): Unit =  {
		require(maxIters > 10 && maxIters < MAX_NUM_ITERS, 
				s"LogBinRegression.check Maximum number of iteration $maxIters is out of bounds")
		require(eta > ETA_LIMITS._1 && eta < ETA_LIMITS._2, 
				s"LogBinRegression.check  Gradient slope $eta + is out of bounds")
		require(eps > EPS_LIMITS._1 && eps < EPS_LIMITS._2, 
				s"LogBinRegression.check  Convergence criteria $eps is out of bounds")
		require( !labels.isEmpty, 
				"LogBinRegression.check  Cannot train with undefined set of observations")
	}
}


// ----------------------  EOF ------------------------------------
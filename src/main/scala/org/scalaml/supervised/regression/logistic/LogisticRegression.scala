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
 * Version 0.98
 */
package org.scalaml.supervised.regression.logistic

import scala.util.{Try, Success, Failure}
	// 3rd party libraries
import org.apache.log4j.Logger
import org.apache.commons.math3.fitting.leastsquares.LeastSquaresOptimizer.Optimum
import org.apache.commons.math3.optim.{SimpleVectorValueChecker, PointVectorValuePair, ConvergenceChecker}
import org.apache.commons.math3.linear.{RealVector, RealMatrix, MatrixUtils,Array2DRowRealMatrix, ArrayRealVector, DiagonalMatrix}
import org.apache.commons.math3.fitting.leastsquares.{LeastSquaresOptimizer, MultivariateJacobianFunction, LeastSquaresBuilder, LeastSquaresProblem}
import org.apache.commons.math3.util.Pair
import org.apache.commons.math3.optim.ConvergenceChecker
	// ScalaMl classes
import org.scalaml.core.{Matrix, XTSeries}
import org.scalaml.core.Types.ScalaMl
import org.scalaml.util.DisplayUtils
import org.scalaml.core.Design.PipeOperator
import org.scalaml.supervised.regression.RegressionModel
import XTSeries._, ScalaMl._



		/**
		 * <p>Logistic regression classifier. This implementation of the logistic regression does not 
		 * support regularization or penalty terms.
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;
		 * ,Arial,Helvetica,sans-serif;">
		 * The likelihood (conditional probability is computed as 1/(1 + exp(-(w(0) + w(1).x(1) + w(2).x(2) + .. + w(n).x(n))) </span></pre></p>
		 * @constructor Create a logistic regression classifier model.
		 * @throws IllegalArgumentException if the class parameters are undefined. 
		 * @see org.apache.commons.math3.fitting.leastsquares._
		 * @see org.apache.commons.math3.optim._
		 * @param xt Input time series observations.
		 * @param labels Labeled class data used during training of the classifier
		 * @param optimizer Optimization method used to minimmize the loss function during training
		 * @author Patrick Nicolas
		 * @since May 11, 2014
		 * @note Scala for Machine Learning Chapter 6 Regression and regularization / Logistic regression
		 */
final class LogisticRegression[T <% Double](
		xt: XTSeries[Array[T]], 
		labels: Array[Int], 
		optimizer: LogisticRegressionOptimizer) extends PipeOperator[Array[T], Int] {
	
	import LogisticRegression._
  	
	type Feature = Array[T]
	check(xt, labels)
    
	private val logger = Logger.getLogger("LogisticRegression")
	
		// The model is created during the instantiation of the LogicticRegression classifier
		// through training. It is set as None if the model could not be trained.
	private[this] val model: Option[RegressionModel] = Try(train) match {
		case Success(model) => Some(model)
		case Failure(e) => DisplayUtils.none("LogisticRegression", logger, e)
	}
	
		/**
		 * <p>Access the weights of the logistic regression model.</p>
		 * @return Vector of weights if the model has been successfully trained, None otherwise.
		 */
	final def weights: Option[DblVector] = model.map(_.weights)
	
		/**
		 * <p>Access the residual sum of squares of the logistic regression model.</p>
		 * @return rss if the model has been successfully trained, None otherwise.
		 */
	final def rss: Option[Double] = model.map(_.rss)
	
		/**
		 * <p>Test if the model has been trained and is defined.</p>
		 * @return true is the model has been trained, false otherwise
		 */
	final def isModel = model != None
			
		/**
		 * <p>Binary predictor using the Binomial logistic regression and implemented
		 * as a data transformation (PipeOperator). The predictor relies on a margin
		 * error to associated to the outcome 0 or 1.<br>
		 * The condition can be either </p>
		 * @throws MatchError if the model is undefined or has an incorrect size or the input feature 
		 * is undefined
		 * @return PartialFunction of feature of type Array[T] as input and the predicted class as output
		 */
	override def |> : PartialFunction[Feature, Int] = {
		case x: Feature  if( !x.isEmpty && model != None &&  (model.get.size -1 == x.size)) => {	
				// Get the weights ws1 to 2n
			val w = model.get.weights.drop(1)
			
				// Compute the predictive value as z = w0 + w1.x1 + ... wn.xn
			val z = x.zip(w).foldLeft(w(0))((s,xw) => s + xw._1*xw._2)
			if( z > HYPERPLANE) 1 else 0
		}
	}
	
		/**
		 * Compute the sigmoid of the argument
		 */
	@inline
	private def logit(x: Double): Double = 1.0/(1.0 + Math.exp(-x))
	
		/**
		 * Training method for the logistic regression
		 */
	private def train: RegressionModel = {
		val weights0 = Array.fill(xt(0).size +1)(INITIAL_WEIGHT)
          
		/*
		 * <p>Anonymous Class that defines the computation of the value of
		 * the function and its derivative (Jacobian matrix) for all the data points.
		 */
		val lrJacobian = new MultivariateJacobianFunction {
			override def value(w: RealVector): Pair[RealVector, RealMatrix] = {
				require(w != null, 
						"MultivariateJacobianFunction undefined weight for computing the Jacobian matrix")
				require(w.toArray.length == dimension(xt)+1, 
						s"MultivariateJacobianFunction Number of weights ${w.toArray.length} != dimension  ${dimension(xt)+1}")

				val _w = w.toArray 	  
					// computes the pair (function value, derivative value)
					// The derivative is computed as logit(1 - logit)
				val gradient = xt.toArray.map( g => {  
					val exponent = g.zip(_w.drop(1))
									.foldLeft(_w(0))((s,z) => s + z._1*z._2)
						// Applies the logistic function to the dot product of weight and features
					val f = logit(exponent)	
						// return the pair of (logistic value, derivative)
					(f, f*(1.0-f))
				})
	
					// Compute the Jacobian (matrix of first derivatives)
					// using the gradient 
				val jacobian = Array.ofDim[Double](xt.size, weights0.size)
				xt.toArray.zipWithIndex.foreach(xi => {    // 1
					val df: Double = gradient(xi._2)._2
							Range(0, xi._1.size).foreach(j => jacobian(xi._2)(j+1) = xi._1(j)*df)
							jacobian(xi._2)(0) = 1.0
				})

					// Need to return the gradient and Jacobian using Apache
					// Commons math types.
				(new ArrayRealVector(gradient.map(_._1)), new Array2DRowRealMatrix(jacobian))
			}
		}

			/**
			 * Create an instance of the convergence criteria (or exit strategy)
			 * using the Apache Commons Math ConvergenceChecker
			 */
		val exitCheck = new ConvergenceChecker[PointVectorValuePair] {
				/**
				 * Apply a converge criteria using the difference in values
				 * between two consecutive iterations and the number of iterations
				 * not to exceed the maximum allowed.
				 */
			override def converged(
					iteration: Int, 
					prev: PointVectorValuePair, 
					current: PointVectorValuePair): Boolean =  {
					// Compute the square root of the sum of squared error
				val delta = prev.getValue.zip(current.getValue).foldLeft(0.0)( (s, z) => { 
					val diff = z._1 - z._2
							s + diff*diff 
				})
					// This is a very simple and commonly used convergence criteria
				Math.sqrt(delta) < optimizer.eps && iteration >= optimizer.maxIters
			}
		}

			/**
			 * The Apache Commons Math lib. require to set up the optimization using
			 * A builder. The Builder in turn, generates a Least Square problem 'lsp'
			 */
		val builder = new LeastSquaresBuilder
		val lsp = builder.model(lrJacobian)
							.weight(MatrixUtils.createRealDiagonalMatrix(Array.fill(xt.size)(1.0))) 
							.target(labels)
							.checkerPair(exitCheck)
							.maxEvaluations(optimizer.maxEvals)
							.start(weights0)
							.maxIterations(optimizer.maxIters)
							.build
							
				// The least square problem is the input to the selected optimizer
		val optimum = optimizer.optimize(lsp)
		RegressionModel(optimum.getPoint.toArray, optimum.getRMS)
	}
}


	/**
	 * <p>Companion object for the logistic regression. The singleton is used
	 * for conversion between Apache Common Math Pair Scala tuple and vice versa.
	 * The singleton is also used to define the constructors
	 * @author Patrick Nicolas
	 * @since May 11, 2014
	 * @note Scala for Machine Learning Chapter 6 Regression and regularization/Logistic regression
	 */
object LogisticRegression {
		
		/**
		 * Class class discriminant for the logit function
		 */
	final val INITIAL_WEIGHT = 0.5
	
		/**
		 * Bias or margin used for one of the class in the logistic regression
		 */
	final val MARGIN = 0.01
	
		/**
		 * Adjusted class discriminant value for the linear exponent fo the logit
		 */
	final val HYPERPLANE = - Math.log(1.0/(MARGIN + INITIAL_WEIGHT) -1)
 
		/**
		 * Convenient implicit conversion between Apache Commons Math pair
		 * and parameterized tuples
		 */
	implicit def pairToTuple[U, V](pair: Pair[U, V]): (U,V) = (pair._1, pair._2)
	
		/**
		 * Convenient implicit conversion between Apache Commons a tuple of
		 * (RealVector, RealMatrix) and a Apache commons math Pair.
		 */
	implicit def tupleToPair[RealVector, RealMatrix](
			pair: (RealVector,RealMatrix)): Pair[RealVector,RealMatrix] 
		= new Pair[RealVector,RealMatrix](pair._1, pair._2)

		/**
		 * Default constructor for the logistic regression
		 * @param xt Input time series observations.
		 * @param labels Labeled class data used during training of the classifier
		 * @param optimizer Optimization method used to minimmize the loss function during training
		 */
	def apply[T <% Double](
			xt: XTSeries[Array[T]], 
			labels: Array[Int], 
			optimizer: LogisticRegressionOptimizer): LogisticRegression[T] =
		new LogisticRegression[T](xt, labels, optimizer)
  	    	
	private def check[T <% Double](xt: XTSeries[Array[T]], labels: Array[Int]): Unit = {
		require( !xt.isEmpty,
				"Cannot compute the logistic regression of undefined time series")
		require(xt.size == labels.size, 
				s"Size of input data ${xt.size} is different from size of labels ${labels.size}")
   }
}

// --------------------------------------  EOF -------------------------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.94
 */
package org.scalaml.supervised.regression.logistic


import org.scalaml.core.XTSeries
import org.scalaml.core.types.ScalaMl._
import org.scalaml.util.Matrix
import scala.util.Random
import org.apache.commons.math3.fitting.leastsquares.LeastSquaresOptimizer.Optimum
import org.apache.commons.math3.optim.{SimpleVectorValueChecker, PointVectorValuePair, ConvergenceChecker}
import org.apache.commons.math3.linear.{RealVector, RealMatrix, MatrixUtils,Array2DRowRealMatrix, ArrayRealVector}
import org.apache.commons.math3.fitting.leastsquares.{LeastSquaresOptimizer, MultivariateJacobianFunction, LeastSquaresBuilder, LeastSquaresProblem}
import org.apache.commons.math3.util.Pair
import org.apache.commons.math3.optim.ConvergenceChecker
import LogisticRegression._
import org.apache.commons.math3.exception.{ConvergenceException, DimensionMismatchException, TooManyEvaluationsException, TooManyIterationsException, MathRuntimeException}
import org.scalaml.core.design.PipeOperator
import org.scalaml.supervised.regression.RegressionModel
import scala.util.{Try, Success, Failure}
import XTSeries._
import org.apache.log4j.Logger
import org.scalaml.util.Display
import org.apache.commons.math3.linear.DiagonalMatrix
import scala.language.implicitConversions

class LogisticRegression[T <% Double](val xt: XTSeries[Array[T]], 
		                                val labels: Array[Int], 
										val optimizer: LogisticRegressionOptimizer) extends PipeOperator[Array[T], Int] {
	type Feature = Array[T]
	validate(xt, labels, optimizer)
    
	private val logger = Logger.getLogger("LogisticRegression")
	private[this] val model: Option[RegressionModel] = {
	    Try(train)
		match {
			case Success(m) => Some(m)
			case Failure(e) => Display.error("LogisticRegression", logger, e); None
		}
	}
	
		/**
		 * <p>Access the weights of the logistic regression model.</p>
		 * @return Vector of weights if the model has been successfully trained, None otherwise.
		 */
	final def weights: Option[DblVector] = model match {
	   case Some(m) => Some(m.weights)
	   case None => None
	}
	
	final def rss: Option[Double] = model match {
	   case Some(m) => Some(m.rss)
	   case None => None
	}
			/**
			 * <p>Binary predictor using the Binomial logistic regression and implemented
			 * as a data transformation (PipeOperator). The predictor relies on a margin
			 * error to associated to the outcome 0 or 1.</p>
			 * @param x new data point to classify as 0 or 1
			 * @return 0 if the logit value is close to 0, 1 otherwise, None if the model could not be trained
			 * @throws IllegalArgumentException if the data point is undefined
			 * @throws DimensionMismatchException if the dimension of the data point x is incorrect
			 */
	override def |> : PartialFunction[Feature, Int] = {
		case x: Feature  if(x != null && model != None && model.get.size -1 != x.size) => {				
		  val w = model.get.weights
		  val z = x.zip(w.drop(1)).foldLeft(w(0))((s,xw) => s + xw._1*xw._2)
		  if( logit(z) > 0.5 + MARGIN) 1 else 0
		}
	}
	
	@inline
	private def logit(x: Double): Double = 1.0/(1.0 + Math.exp(-x))
	
	
	final val initWeight = 0.5
	private def train: RegressionModel = {
        val weights0 = Array.fill(xt(0).size +1)(initWeight)
          
        		/**
        		 * <p>Anonymous Class that defines the computation of the value of
        		 * the function and its derivative (Jacobian matrix) for all the data points.
        		 */
	  	 val lrJacobian = new MultivariateJacobianFunction {

            override def value(w: RealVector): Pair[RealVector, RealMatrix] = {
		  	  require(w != null && w.toArray.length == dimension(xt)+1, "Incorrect size of weight for computing the Jacobian matrix")
		  	  
		  	  val _w = w.toArray 	  
		  			  // computes the pair (function value, derivative value)
		  	  val gradient = xt.toArray.map( g => {  
		  	  	 val exponent = g.zip(_w.drop(1))
		  	  	                 .foldLeft(_w(0))((s,z) => s + z._1*z._2)
		  	  	 val f = logit(exponent)
		  	  	 (f, f*(1.0-f))
		  	  })
		  	  
		  	 val jacobian = Array.ofDim[Double](xt.size, weights0.size)
	         xt.toArray.zipWithIndex.foreach(xi => {    // 1
	        	 val df: Double = gradient(xi._2)._2
	        	 Range(0, xi._1.size).foreach(j => jacobian(xi._2)(j+1) = xi._1(j)*df)
	        	 jacobian(xi._2)(0) = 1.0
	         })
	          
		     (new ArrayRealVector(gradient.map(_._1)), new Array2DRowRealMatrix(jacobian))
		   }
		}

        	
	    val exitCheck = new ConvergenceChecker[PointVectorValuePair] {
	    	override def converged(iteration: Int, prev: PointVectorValuePair, current: PointVectorValuePair): Boolean =  {
	    	  val delta = prev.getValue.zip(current.getValue).foldLeft(0.0)( (s, z) => { 
	    	  	  val diff = z._1 - z._2
	    	  	  s + diff*diff 
	    	  })
	    	  Math.sqrt(delta) < optimizer.eps && iteration >= optimizer.maxIters
	    	}
	    }

	    val builder = new LeastSquaresBuilder
        val lsp = builder.model(lrJacobian)
                        .weight(MatrixUtils.createRealDiagonalMatrix(Array.fill(xt.size)(1.0))) 
     //   .weight(new DiagonalMatrix(weights0)) 
                         .target(labels)
                         .checkerPair(exitCheck)
                         .maxEvaluations(optimizer.maxEvals)
                         .start(weights0)
                         .maxIterations(optimizer.maxIters)
                         .build
                                 
        val optimum = optimizer.optimize(lsp)
        println("Optimum: ")
        RegressionModel(optimum.getPoint.toArray, optimum.getRMS)
	}
	
    private def validate(xt: XTSeries[Array[T]], labels: Array[Int], optimizer: LogisticRegressionOptimizer): Unit = {
	  require(xt != null && xt.size > 0, "Cannot compute the logistic regression of undefined time series")
	  require(xt.size == labels.size, "Size of input data " + xt.size + " is different from size of labels " + labels.size)
      require(optimizer != null, "Cannot execute a logistic regression with undefined optimizer")
   }
	
}


	/**
	 * <p>Companion object for the logistic regression. The singleton is used
	 * for conversion between Apache Common Math Pair Scala tuple and vice versa.
	 * The singleton is also used to define the constructors
	 */
object LogisticRegression {
   final val MARGIN = 0.01
   
   implicit def pairToTuple[U, V](pair: Pair[U, V]): (U,V) = (pair._1, pair._2)
   implicit def tupleToPair[RealVector, RealMatrix](pair: (RealVector,RealMatrix)): Pair[RealVector,RealMatrix] = new Pair[RealVector,RealMatrix](pair._1, pair._2)
  	    
   def apply[T <% Double](xt: XTSeries[Array[T]], labels: Array[Int], optimizer: LogisticRegressionOptimizer): LogisticRegression[T] =
  	    new LogisticRegression[T](xt, labels, optimizer)
}


// --------------------------------------  EOF -------------------------------------------------------
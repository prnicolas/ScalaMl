/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.supervised.regression.logistic


import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
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
import org.scalaml.workflow.PipeOperator
import org.scalaml.supervised.regression.RegressionModel

		/**
		 * <p>Logistic regression for multivariate logistic regression. The implementation
		 * used the GaussNewton optimizer to extract the weights or coefficient of the
		 * logistic regression. The generic form of the logistic regression is<br>
		 * y 1/(1 + exp(-(w0 + w1.x0 + w2.x1 ..)).<br>
		 * The class relies on the optimization routine implemented in the Apache Commons
		 * Math library math3.optim and math3.fitting.leastsquares packages.</p>
		 * @param xt multi-dimensional time series 
		 * @param y observed/labelled data related to the time series {0, 1}
		 * @param maxIters maximum number of iterations
		 * @param maxEvals maximum number of evaluations
		 * @throws IllegalArgumentException if the arguments are improperly defined
		 * 
		 * @author Patrick Nicolas
		 * @since April 24, 2014
		 * @note Scala for Machine Learning
		 */
import XTSeries._
final class LogisticRegression[T <% Double](val xt: XTSeries[Array[T]], 
		                                val labels: Array[Int], 
										val optimizer: LogisticRegressionOptimizer) extends PipeOperator[Array[T], Int] {
	validate(xt, labels, optimizer)

	private val model: Option[RegressionModel] = {
		try {
			val weightAcc = train
			Some(RegressionModel(weightAcc._1, weightAcc._2))
		}
		catch {
			case e: ConvergenceException => println("Algorithm failed to converge " + e.toString); None
			case e: DimensionMismatchException =>  println("Jacobian and data vector mismatched dimension " + e.toString); None
			case e: TooManyEvaluationsException => println("Too many evaluations " + e.toString); None
			case e: TooManyIterationsException => println("Too many iterations " + e.toString); None
			case e: MathRuntimeException =>  println("Run time exception  " + e.toString); None
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
	
	def rms: Option[Double] = model match {
	   case Some(m) => Some(m.accuracy)
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
	override def |> (feature: Array[T]): Option[Int] = model match {
	  case Some(m) => {
		 if( feature != null || m.size +1 != feature.size) 
			throw new IllegalStateException("Size of input data for prediction " + feature.size + " should be " + (m.size -1))
				
		 val z= - feature.zip(m.weights).foldLeft(0.0)((s,xw) => s + xw._1*xw._2)
		 Some(if( Math.abs(1.0 - logit(z)) < MARGIN ) 1 else 0)
	   }
	   case None => None	
	}
	
	@inline
	private def logit(x: Double): Double = 1.0/(1.0 + Math.exp(-x))
	
	
	private def train: (DblVector, Double) = {
        val _weights = Array.fill(xt(0).size +1)(0.5)
          
        		/**
        		 * <p>Anonymous Class that defines the computation of the value of
        		 * the function and its derivative (Jacobian matrix) for all the data points.
        		 */
	  	 val lrJacobian = new MultivariateJacobianFunction {

            override def value(w: RealVector): Pair[RealVector, RealMatrix] = {
		  	  require(w != null && w.toArray.length == dimension(xt)+1, "Incorrect size of weight for computing the Jacobian matrix")
		  	  
		  	  val nW = w.toArray 	  
		  			  // computes the pair (function value, derivative value)
		  	  val gradient = xt.toArray.map( g => {  
		  	  	 val exponent = g.zip(nW.drop(0)).foldLeft(nW(0))((s,z) => s + z._1*z._2)
		  	  	 val f = logit(exponent)
		  	  	 (f, f*(1-f))
		  	  })
		  	  
		  	 val jacobian = Array.ofDim[Double](xt.size, _weights.size)
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
	    	  val delta = prev.getValue.zip(current.getValue).foldLeft(0.0)( (s, z) => { val diff = z._1 - z._2;  s + diff*diff} )
	    	  Math.sqrt(delta) < optimizer.eps &&  iteration >= optimizer.maxIters
	    	}
	    }

	    val builder = new LeastSquaresBuilder
        val lsp = builder.model(lrJacobian)
                         .weight(MatrixUtils.createRealDiagonalMatrix(Array.fill(xt.size)(1.0))) 
                         .target(labels)
                         .checkerPair(exitCheck)
                         .maxEvaluations(optimizer.maxEvals)
                         .start(_weights)
                         .maxIterations(optimizer.maxIters)
                         .build
                                 
        val optimum = optimizer.optimize(lsp)
        println("Optimum: ")
        (optimum.getPoint.toArray, optimum.getRMS)
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
   final val MARGIN = 0.1
   
   implicit def pairToTuple[U, V](pair: Pair[U, V]): (U,V) = (pair._1, pair._2)
   implicit def tupleToPair[RealVector, RealMatrix](pair: (RealVector,RealMatrix)): Pair[RealVector,RealMatrix] = new Pair[RealVector,RealMatrix](pair._1, pair._2)
  	    
   def apply[T <% Double](xt: XTSeries[Array[T]], labels: Array[Int], optimizer: LogisticRegressionOptimizer): LogisticRegression[T] =
  	    new LogisticRegression[T](xt, labels, optimizer)
}


// --------------------------------------  EOF -------------------------------------------------------
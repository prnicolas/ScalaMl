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


case class LogisticRegressionOptimizer(val maxIters: Int, 
		                               val maxEvals: Int, 
		                               val eps: Double, 
		                               private val lsOptimizer: LeastSquaresOptimizer) {
  require(maxIters > 0 && maxIters < maxEvals, 
			   "Maximum number of iterations " + maxIters + " exceeds maximum number of evaluations" + maxEvals)
  require( eps > MIN_EPS && eps < MAX_EPS, "eps for the optimization of the logistic regression " + eps + " is out of range")
  require(lsOptimizer != null, "Least squares optimizer for the logistic regression is undefined")
  
  def optimize(lsProblem: LeastSquaresProblem): Optimum = {
  	  require(lsProblem != null, "Cannot perform a Least Squares optimization with undefined Least square problem")
  	  lsOptimizer.optimize(lsProblem)
  }
}

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
		 * @exception IllegalArgumentException if the arguments are improperly defined
		 * 
		 * @author Patrick Nicolas
		 * @date April 24, 2014
		 * @project Scala for Machine Learning
		 */
import XTSeries._
class LogisticRegression[T <% Double](	val xt: XTSeries[Array[T]], 
		                                val y: Array[Int], 
										val optimizer: LogisticRegressionOptimizer) extends PipeOperator[Array[T], Int] {
	
	require(xt != null && xt.size > 0, "Cannot compute the logistic regression of undefined time series")
	require(xt.size == y.size, "Size of input data " + xt.size + " is different from size of labels " + y.size)
    require(optimizer != null, "Cannot execute a logistic regression with undefined optimizer")
	
	private val model: Option[(DblVector, Double)] = {
		try {
			Some(train)
		}
		catch {
			case e: ConvergenceException => println("Algorithm failed to converge " + e.toString); None
			case e: DimensionMismatchException =>  println("Jacobian and data vector mismatched dimension " + e.toString); None
			case e: TooManyEvaluationsException => println("Too many evaluations " + e.toString); None
			case e: TooManyIterationsException => println("Too many evaluations " + e.toString); None
			case e: MathRuntimeException =>  println("Run time exception  " + e.toString); None
		}
	}
		
	
	
	def weights: Option[DblVector] = {
		model match {
			case Some(wr) => Some(wr._1)
			case None => None
		}
	}
	
	def rms: Option[Double] = {
	    model match {
			case Some(wr) => Some(wr._2)
			case None => None
		}
	}
			/**
			 * <p>Binary predictor using the Binomial logistic regression and implemented
			 * as a data transformation (PipeOperator). The predictor relies on a margin
			 * error to associated to the outcome 0 or 1.</p>
			 * @param x new data point to classify as 0 or 1
			 * @return 0 if the logit value is close to 0, 1 otherwise, None if the model could not be trained
			 * @exception IllegalArgumentException if the data point is undefined
			 * @exception DimensionMismatchException if the dimension of the data point x is incorrect
			 */
	override def |> (x: Array[T]): Option[Int] = model match {
	  case Some(m) => {
		 if( x != null || m._1.size +1 != x.size) 
			throw new IllegalStateException("Size of input data for prediction " + x.size + " should be " + (m._1.size -1))
				
		 val z= - x.zip(m._1).foldLeft(0.0)((s,xw) => s + xw._1*xw._2)
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
                         .target(y)
                         .checkerPair(exitCheck)
                         .maxEvaluations(optimizer.maxEvals)
                         .start(_weights)
                         .maxIterations(optimizer.maxIters)
                         .build
                                 
        val optimum = optimizer.optimize(lsp)
        println("Optimum: ")
        (optimum.getPoint.toArray, optimum.getRMS)
	}
}


object LogisticRegression {
   final val MARGIN = 0.1
   final val MIN_EPS = 1e-32
   final val MAX_EPS = 1.0
   
   implicit def pairToTuple[U, V](pair: Pair[U, V]): (U,V) = (pair._1, pair._2)
   implicit def tupleToPair[RealVector, RealMatrix](pair: (RealVector,RealMatrix)): Pair[RealVector,RealMatrix] = new Pair[RealVector,RealMatrix](pair._1, pair._2)
  	    
   def apply[T <% Double](xt: XTSeries[Array[T]], y: Array[Int], optimizer: LogisticRegressionOptimizer): LogisticRegression[T] =
  	    new LogisticRegression[T](xt, y, optimizer)
}


// --------------------------------------  EOF -------------------------------------------------------
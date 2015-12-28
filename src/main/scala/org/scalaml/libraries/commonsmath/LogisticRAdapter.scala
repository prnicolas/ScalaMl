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
package org.scalaml.libraries.commonsmath

import scala.language.implicitConversions


import org.apache.commons.math3.linear._
import org.apache.commons.math3.exception.{MathIllegalArgumentException, MathRuntimeException, OutOfRangeException}
import org.apache.commons.math3.fitting.leastsquares.MultivariateJacobianFunction
import org.apache.commons.math3.optim.ConvergenceChecker
import org.apache.commons.math3.optim.PointVectorValuePair
import org.apache.commons.math3.util.Pair

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.stats.Loss._
import org.scalaml.supervised.regression.Regression._
import org.scalaml.libraries.commonsmath.CommonsMath._

/**
		 * Implicit conversion from internal primitive types DblArray and DblMatrix to Apache 
		 * Commons Math types.
		 * @author Patrick Nicolas
		 * @since January 23, 2014 0.98
		 * @version 0.98.1
		 * @see Scala for Machine Learning Chapter 3 Data pre-processing/Time series
		 */
object LogisticRAdapter {
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
			pair: (RealVector,RealMatrix)): Pair[RealVector,RealMatrix] = 
		new Pair[RealVector,RealMatrix](pair._1, pair._2)
	

		
	class RegressionJacobian[T <: AnyVal](
			xv: XVSeries[T], 
			weights0 : DblArray)(implicit h: T => Double) extends MultivariateJacobianFunction {
	  
		override def value(w: RealVector): Pair[RealVector, RealMatrix] = {
			require(w != null, 
				"RegressionJacobian undefined weight for computing the Jacobian matrix")
			require(w.toArray.length == xv.head.length +1, 
				s"RegressionJacobian Number of weights ${w.toArray.length} != dimension  ${xv.head.length+1}")

			val _w = w.toArray 	  
				
					// computes the pair (function value, derivative value)
					// The derivative is computed as logit(1 - logit)
			val gradient = xv.map( g => {  
							// Applies the logistic function to the dot product of weight and features
				val f = logistic( dot(g, w))	
					// return the pair of (logistic value, derivative)
				(f, f*(1.0-f))
			})
	
					// Compute the Jacobian (matrix of first derivatives)
					// using the gradient 
		//	val jacobian = Array.ofDim[Double](xv.size, weights0.size)
					
			val jacobian =
			  xv.zipWithIndex./:(Array.ofDim[Double](xv.size, weights0.length)) {
				case (j, (x,i)) =>
					val df = gradient(i)._2
					x.indices.foreach(n => j(i)(n+1) = x(n)*df)
					j(i)(0) = 1.0
					j
			}

					// Need to return the gradient and Jacobian using Apache Commons math types.
			(new ArrayRealVector(gradient.map(_._1).toArray), new Array2DRowRealMatrix(jacobian))
		}
			
		@inline
		private def logistic(x: Double): Double = 1.0/(1.0 + Math.exp(-x))
	}

			/**
			 * Create an instance of the convergence criteria (or exit strategy)
			 * using the Apache Commons Math ConvergenceChecker
			 */
	class RegressionConvergence(optimizer: LogisticRegressionOptimizer) 
			extends ConvergenceChecker[PointVectorValuePair] {

			/**
			 * Apply a converge criteria using the difference in values
			 * between two consecutive iterations and the number of iterations
			 * not to exceed the maximum allowed.
			 */
		override def converged(
				iteration: Int, 
				prev: PointVectorValuePair, 
				current: PointVectorValuePair): Boolean =  
			
					// This is a very simple and commonly used convergence criteria
			sse(prev.getValue, current.getValue) < optimizer.eps && iteration >= optimizer.maxIters
	}


}
		


// ---------------------------  EOF -----------------------------
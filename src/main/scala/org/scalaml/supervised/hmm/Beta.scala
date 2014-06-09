/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 * 
 */
package org.scalaml.supervised.hmm



import org.scalaml.util.Matrix
import org.scalaml.core.Types

		/**
		 * <p>Implementation of the Beta or backward pass of the 
		 * HMM algorithm to compute the probability of a sequence of observations. The beta
		 * matrix is computed as part of the instantiation of the class.</p>
		 * @param lambdaB Lambda (pi, A, B) model for the HMM
		 * @param parms parameters used in any of the three canonical form of the HMM
		 * @param obsB: Array of observations as integer (categorical data)
		 * @exception IllegalArgumentException if lmbda, params and observations are undefined
		 * @author Patrick Nicolas
		 * @date March 14, 2014
		 */
final class Beta(val lambdaB: HMMLambda, val obsB: Array[Int]) extends Pass(lambdaB, obsB) {
	val complete = {
		try {
		    alphaBeta = Matrix[Double](lambda.d._T, lambda.d._N)	
		    alphaBeta += (lambda.d_1, 1.0)
		    normalize(lambda.d_1)
		    recurse
		    true
		}
		catch {
			case e: ArithmeticException => println("Failed beta computation " + e.toString()); false
			case e: RuntimeException => println("Failed beta computation " + e.toString()); false
		}
	}
	
    private def recurse: Unit = 
	   (lambda.d._T-2 to 0 by -1).foreach( t =>{updateBeta(t); normalize(t) })
	  	
	private  def updateBeta(t: Int): Unit =
  	   lambda.d.foreachN( i => { 
  	 	  alphaBeta += (t, i, lambda.beta(alphaBeta(t+1, i), i, obs(t+1)))	
  	   })
}


		/**
		 * Companion object for the Beta pass that defines the constructor apply
		 */
object Beta {
	def apply(lambda: HMMLambda, obs: Array[Int]): Beta = new Beta(lambda, obs)
}




// --------------------------------  EOF -------------------------------------
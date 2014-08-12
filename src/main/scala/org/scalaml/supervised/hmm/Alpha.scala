/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.supervised.hmm


import org.scalaml.util.Matrix
import org.scalaml.core.Types


		/**
		 * <p>Implementation of the Alpha pass (forward algorithm). The Alpha parameter 
		 * is computed during instantiation.</p>
		 * @param lambdaA Lambda (pi, A, B) model for the HMM
		 * @param params parameters used in any of the three canonical form of the HMM
		 * @param obsA: Array of observations as integer (categorical data)
		 * @throws IllegalArgumentException if lambda, params and observations are undefined
		 * @author Patrick Nicolas
		 * @since March 13, 2014
		 */
final class Alpha(val lambdaA: HMMLambda, val obsA: Array[Int]) extends Pass(lambdaA, obsA) {
	/**
	 * Alpha variable computed through the recursive forward algorithm
	 */
  val alpha = { alphaBeta = lambda.initAlpha(labels); normalize(0); recurse }

  
  	/**
  	 * <p>Compute the sum  log of the conditional probability p(X|Y) for each feature. Exception
  	 * thrown during the comptutation are caught in the client code.</p>
  	 * @return sum of the logarithm of the conditional probability p(xi|Y)
  	 */
  def logProb: Double = lambda.d.rt.foldLeft(Math.log(alpha))((s, t) => s + Math.log(ct(t)))
  
  private def recurse: Double = {	 
	 lambda.d.foreachT(t => {updateAlpha(t); normalize(t)})
	 lambda.d.rn.foldLeft(0.0)((s, k) => s + alphaBeta(lambda.d_1, k))
  }

   
  private def updateAlpha(t: Int): Unit = 
  	lambda.d.foreachM( i => { 
  	   alphaBeta += (t, i, lambda.alpha(alphaBeta(t-1, i), i, labels(t))) 
    })
}


		/**
		 * Companion object for the Alpha pass that defines the constructor applhy
		 * @author Patrick Nicolas
		 * @since March 13, 2014
		 */
object Alpha {
  def apply(lambda: HMMLambda, obs: Array[Int]): Alpha = new Alpha(lambda,obs)
}



// --------------------------------  EOF -------------------------------------
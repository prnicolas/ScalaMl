/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
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
		 * @throws IllegalArgumentException if lambda, or  observations are undefined
		 * @author Patrick Nicolas
		 * @since March 13, 2014
		 */
final class Alpha(val lambdaA: HMMLambda, val obsA: Array[Int]) extends Pass(lambdaA, obsA) {
	/**
	 * Alpha variable computed through the recursive forward algorithm
	 */
  val alpha = { 
  	alphaBeta = lambda.initAlpha(labels)
  	normalize(0)
  	sumUp
  }

  
  	/**
  	 * <p>Compute the sum  log of the conditional probability p(X|Y) for each feature. Exception
  	 * thrown during the comptutation are caught in the client code.</p>
  	 * @return sum of the logarithm of the conditional probability p(xi|Y)
  	 */
  def logProb: Double = HMMDim.foldLeft(lambda.dim._T, (s, t) => s + Math.log(ct(t)), Math.log(alpha))
  
  private def sumUp: Double = {	 
	 HMMDim.foreach(lambda.dim._T, t => {
		updateAlpha(t)
		normalize(t)
	 })
	 HMMDim.foldLeft(lambda.dim._N, (s, k) => s + alphaBeta(lambda.d_1, k))
  }

   
  private def updateAlpha(t: Int): Unit = 
  	HMMDim.foreach(lambda.dim._N, i => 
  	   alphaBeta += (t, i, lambda.alpha(alphaBeta(t-1, i), i, labels(t))) 
    )
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
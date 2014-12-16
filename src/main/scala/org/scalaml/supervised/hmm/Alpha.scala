/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.2
 */
package org.scalaml.supervised.hmm


import org.scalaml.core.Matrix
import org.scalaml.core.Types.ScalaMl._
import HMMConfig._


		/**
		 * <p>Implementation of the Alpha pass (forward algorithm). The Alpha parameter 
		 * is computed during instantiation.</p> 
		 * @constructor Create a Alpha pass for the evaluation canonical form of the hidden 
		 * Markov model (HMM). 
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 * probabilities, the state-transition probabilities matrix and the emission proabilities matrix.
		 * @param obs Array of observations as integer (categorical data
		 * @param alpha Alpha coefficient for the forward pass in the evaluation form of HMM
		 * @throws IllegalArgumentException if lambda, or  observations are undefined
		 * @see org.scalaml.supervised.hmm.Pass
		 * 
		 * @author Patrick Nicolas
		 * @since March 13, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model 
		 * - Evaluation
		 */
final protected class Alpha(lambda: HMMLambda, obs: Array[Int]) extends Pass(lambda, obs) {
	/**
	 * Alpha variable computed through the recursive forward algorithm
	 */
	val alpha: Double = { 
		alphaBeta = lambda.initAlpha(obs)
		normalize(0)
		sumUp
	}

  
		/**
		 * <p>Compute the sum  log of the conditional probability p(X|Y) for each feature. Exception
		 * thrown during the computation are caught in the client code.</p>
		 * @return sum of the logarithm of the conditional probability p(xi|Y)
		 */
	def logProb: Double = foldLeft(lambda.getT, 
		(s, t) => s + Math.log(ct(t)), Math.log(alpha))
			
	private def sumUp: Double = {	 
		foreach(1, lambda.getT, t => {
			updateAlpha(t)
			normalize(t)
		})
		foldLeft(lambda.getN, (s, k) => s + alphaBeta(lambda.d_1, k))
	}

   
	private def updateAlpha(t: Int): Unit = 
		foreach(lambda.getN, i => {
			val newValue = lambda.alpha(alphaBeta(t-1, i), i, obs(t))
			alphaBeta += (t, i, lambda.alpha(alphaBeta(t-1, i), i, obs(t))) 
		})
}

		/**
		 * Companion object for the Alpha pass that defines the constructor applhy
		 * @author Patrick Nicolas
		 * @since March 13, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model /
		 * Evaluation
		 */
object Alpha {
		/**
		 * Default constructor for the class Alpha
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 * probabilities, the state-transition probabilities matrix and the emission proabilities matrix.
		 * @param obs Array of observations as integer (categorical data
		 * @param alpha Alpha coefficient for the forward pass in the evaluation form of HMM
		 */
	def apply(lambda: HMMLambda, obs: Array[Int]): Alpha = new Alpha(lambda,obs)
}


// --------------------------------  EOF -------------------------------------
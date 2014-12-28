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
package org.scalaml.supervised.hmm


import org.scalaml.core.Matrix
import org.scalaml.core.Types.ScalaMl._
import HMMConfig._


		/**
		 * <p>Implementation of the Alpha pass (forward algorithm). The Alpha parameter 
		 * is computed during instantiation. It is the probability of being in state S(i) given 
		 * a sequence of observation {0, 1,   t}</p> 
		 * @constructor Create a Alpha pass for the evaluation canonical form of the hidden 
		 * Markov model (HMM). 
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 * probabilities, the state-transition probabilities matrix and the emission probabilities matrix.
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
	 * Alpha variable computed through the recursive forward algorithm. The value is 
	 * computed the initial value alpha (M1 formula). normalize the alpha values (M2)
	 * and finally invoke the summation of the normalized value (M3).
	 * @see Chapter 7 Sequential Data Models / Hidden Markov model / Evaluation  / Alpha pass
	 */
	val alpha: Double = { 
		alphaBeta = lambda.initAlpha(obs)
		normalize(0)
		sumUp
	}

  
		/**
		 * <p>Compute the sum  log of the conditional probability p(X|Y) for each feature. Exception
		 * thrown during the computation are caught in the client code.[M4 formula]</p>
		 * @return sum of the logarithm of the conditional probability p(xi|Y)
		 * @see Chapter 7 Sequential Data Models / Hidden Markov model / Evaluation / Alpha pass
		 */
	def logProb: Double = foldLeft(lambda.getT, (s, t) => s + Math.log(ct(t)), Math.log(alpha))

			/*
			 * Compute the alpha(t) = SUM{ alpha(t-1)).a.b (M3), then normalized to generate c(t)
			 * value and finally compute an estimate for the alpha(t)
			 */
	private def sumUp: Double = {	 
		foreach(1, lambda.getT, t => {
			updateAlpha(t)		// Implements first equation of M3
			normalize(t)			// Normalized wit the sum of alpha(i), t 0 -> N-1
		})
		foldLeft(lambda.getN, (s, k) => s + alphaBeta(lambda.d_1, k))
	}

		/*
		 * Update the value of alpha at observation t by summation of alpha(i).a(i).b(i) 
		 * for the previous observation t -1 across all the N states
		 */
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
		 * probabilities, the state-transition probabilities matrix and the emission probabilities matrix.
		 * @param obs Array of observations as integer (categorical data
		 * @param alpha Alpha coefficient for the forward pass in the evaluation form of HMM
		 */
	def apply(lambda: HMMLambda, obs: Array[Int]): Alpha = new Alpha(lambda,obs)
}


// --------------------------------  EOF -------------------------------------
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
package org.scalaml.supervised.hmm.eval

import scala.util.Try

import org.scalaml.supervised.hmm.{HMMConfig, HMMModel, HMMTreillis}
import HMMConfig._


		/**
		 * Implementation of the Alpha pass (forward algorithm). The Alpha parameter 
		 * is computed during instantiation. It is the probability of being in state S(i) given 
		 * a sequence of observation {0, 1,   t} 
		 * @constructor Create a Alpha pass for the evaluation canonical form of the hidden 
		 * Markov model (HMM). 
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 * probabilities, the state-transition probabilities matrix and the emission probabilities 
		 * matrix.
		 * @param obsSeq Array of observations as integer (categorical data)
		 * @throws IllegalArgumentException if lambda, or  observations are undefined
		 * 
		 * @author Patrick Nicolas
		 * @since March 13, 2014
		 * @see Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model 
		 * - Evaluation
		 */
final protected class Alpha(lambda: HMMModel, obsSeq: Vector[Int]) 
		extends HMMTreillis(lambda.numObs, lambda.numStates) {


	/**
	 * Alpha variable computed through the recursive forward algorithm. The value is 
	 * computed the initial value alpha (M1 formula). normalize the alpha values (M2)
	 * and finally invoke the summation of the normalized value (M3).
	 * @see Chapter 7 Sequential Data Models / Hidden Markov model / Evaluation  / Alpha pass
	 */
	val alpha: Double = Try { 
		treillis = lambda.setAlpha(obsSeq)
		normalize(0)
		sumUp
	}.getOrElse(Double.NaN)


	@inline
	override def isInitialized: Boolean = alpha != Double.NaN
	
		/**
		 * Compute the sum  log of the conditional probability p(X|Y) for each feature. Exception
		 * thrown during the computation are caught in the client code.[M4 formula]
		 * @return sum of the logarithm of the conditional probability p(xi|Y)
		 * @see Chapter 7 Sequential Data Models / Hidden Markov model / Evaluation / Alpha pass
		 */
	def logProb: Double = /:(lambda.numObs, (s, t) => s + Math.log(ct(t)), Math.log(alpha))

			/*
			 * Compute the alpha(t) = SUM{ alpha(t-1)).a.b (M3), then normalized to generate c(t)
			 * value and finally compute an estimate for the alpha(t)
			 */
	private def sumUp: Double = {	 
		foreach(1, lambda.numObs, t => {
			updateAlpha(t)		// Implements first equation of M3
			normalize(t)			// Normalized wit the sum of alpha(i), t 0 -> N-1
		})
		/:(lambda.numStates, (s, k) => s + treillis(lambda.numObs-1, k))
	}

		/*
		 * Update the value of alpha at observation t by summation of alpha(i).a(i).b(i) 
		 * for the previous observation t -1 across all the N states
		 */
	private def updateAlpha(t: Int): Unit = 
		foreach(lambda.numStates, i => 
			treillis += (t, i, lambda.getAlphaVal(treillis(t-1, i), i, obsSeq(t))) 
		)
}

		/**
		 * Companion object for the Alpha pass that defines the constructor apply
		 * @author Patrick Nicolas
		 * @since March 13, 2014
		 * @see Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model /
		 * Evaluation
		 */
object Alpha {
  
		/**
		 * Default constructor for the class Alpha
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 * probabilities, the state-transition probabilities matrix and the emission probabilities 
		 * matrix.
		 * @param obsSeq Array of observations as integer (categorical data
		 */
	def apply(lambda: HMMModel, obsSeq: Vector[Int]): Alpha = new Alpha(lambda,obsSeq)
}


// --------------------------------  EOF -------------------------------------
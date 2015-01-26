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
 * Version 0.98.1
 */
package org.scalaml.supervised.hmm


import org.scalaml.core.Matrix
import HMMConfig._


		/**
		 * <p>Class that implements the Viterbi algorithm to extract the best sequence
		 * of hidden states in a HMM given a lambda model and a sequence of observations. The 
		 * maximum value of delta is computed recursively during instantiation.</p>
		 *  @constructor Create an instance of the Viterbi algorithm for a predefined Lambda model 
		 *  and set of observations.
		 *  @see org.scalaml.hmm.HMMModel
		 *  @see Chapter 7 Sequential Data Models / Hidden Markov model / Evaluation / Viterbi
		 *  @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 *  probabilities, the state-transition probabilities matrix and the emission probabilities 
		 *  matrix.
		 *  @param obs Array of observations as integer (categorical data)
		 *  
		 *  @author Patrick Nicolas
		 *  @since January 16, 2015
		 *  @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model 
		 *  Decoding
		 */
final protected class ViterbiPath(lambda: HMMLambda, obs: Array[Int]) extends HMMModel(lambda, obs) {
		/**
		 * State of the execution of the decoding form of HMM
		 */
	protected[this] val state = HMMState(lambda)
	
		/**
		 * Pair (prob, states_sequence) where states_sequence is the
		 * the most likely sequence of states given a Lambda model and a sequence
		 * of observations, and prob its likelihood
		 */
	val maxDelta: (Double, Array[Int]) = (recurse(0), state.QStar())

			/*
		 * Recursive computation of psi and delta [M12] and [M14] along the 
		 * observations O(t) t 0 -> T, using the Scala effective tail recursion
		 * THe method uses the notation introduced in "Scala for Machine Learning"
		 * and "Introduction to Machine Learning" by E. Alpaydin.
		 */
	@scala.annotation.tailrec
	private def recurse(t: Int): Double = {
		
			// Initialization of the delta value, return -1.0 in case of error
		if( t == 0)
			initial 

			// then for the subsequent observations ...
		else {	
			// Update the maximum delta value and its state index for the observation t
			Range(0, lambda.getN).foreach( updateMaxDelta(t, _) )
			
			// If we reached the last observation... exit by backtracing the
			// computation of the 
			if( t ==  obs.size-1) {
				val idxMaxDelta = Range(0, lambda.getN).map(i => (i, state.delta(t, i))).maxBy(_._2)
					// Update the Q* value with the index that maximize the delta.A
				state.QStar.update(t+1, idxMaxDelta._1)
				idxMaxDelta._2
			}
			else 
				recurse(t+1)
		}
	}
		/*
		 * Compute the delta value given the initial probabilities pi and the 
		 * first observed data, obs( t = 0).
		 * @return initial value for maximum likelihood (delta) for the first observation t = 0
		 */
	private def initial: Double = {
			// For each of the dimension of the first observation
			// update the index and the delta vale.
		Range(0, lambda.getN).foreach(n => {
			state.psi += (0, n, 0)
			state.delta += (0, n, lambda.pi(n) * lambda.B(n, obs(0)))
		})
			// ... then move to the next (t = 1) observation
		recurse(1)
	}
	
		/*
		 * Update the value of the index psi of the maximum likelihood (maximum delta)
		 * for the observation of index t and to state j
		 */
	private def updateMaxDelta(t: Int, j: Int): Unit = {
			// compute the maximum delta for the states for this observation and
			// the its state index.
		val idxDelta = Range(0, lambda.getN).map(i => (i, state.delta(t-1, i)*lambda.A(i, j)))
				.maxBy(_._2)
		state.psi += (t, j, idxDelta._1)
		state.delta += (t, j, idxDelta._2)
	}
}

	/**
	 * Object companion for the Viterbi algorithm for the extraction of 
	 * best sequences. Implements the constructor - apply
	 * @author Patrick Nicolas
	 * @since March 17, 2014
	 */
object ViterbiPath {

		/**
		 * Default constructor for the Viterbi algorithm
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 * probabilities, the state-transition probabilities matrix and the emission probabilities matrix.
		 * @param obs Array of observations as integer (categorical data)
		 */
	def apply(lambda: HMMLambda, _labels: Array[Int]): ViterbiPath = 
			new ViterbiPath(lambda, _labels)
}


// -------------------------  EOF ----------------------------------------------
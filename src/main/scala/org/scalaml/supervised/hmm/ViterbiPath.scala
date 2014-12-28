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
import HMMConfig._


		/**
		 * <p>Class that implements the Viterbi algorithm to extract the best sequence
		 * of hidden states in a HMM given a lambda model and a sequence of observations. The 
		 * maximum value of delta is computed recursively during instantiation.</p>
		 *  @constructor Create an instance of the Viterbi algorithm for a predefined Lambda model 
		 *  and set of observations.
		 *  @throws IllegalArgumentException if lambda, params and observations are undefined of 
		 *  eps is out of range
		 *  @see org.scalaml.hmm.HMMModel
		 *  @see Chapter 7 Sequential Data Models / Hidden Markov model / Evaluation / Viterbi
		 *  @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 *  probabilities, the state-transition probabilities matrix and the emission probabilities matrix.
		 *  @param obs Array of observations as integer (categorical data)
		 *  
		 *  @author Patrick Nicolas
		 *  @since March 17, 2014
		 *  @note Scala for Machine Learning Chapter 7 Sequential data models/Hidden Markov Model/Decoding
		 */
final protected class ViterbiPath(lambda: HMMLambda, obs: Array[Int]) extends HMMModel(lambda, obs) {

		/**
		 * Maximum value for delta computed by recursion. the computation
		 * throws a Arithmetic or Runtime exception that is to be caught by
		 * the client code
		 */
	val maxDelta = recurse(lambda.getT, 0)
	
		/**
		 * State of the execution of the decoding form of HMM
		 */
	val state = HMMState(lambda)

		/*
		 * Compute the delta value given the initial probabilities pi and the emission
		 * probabilities B. The value psi is initialized as null [M13]
		 * @param ti tuple (observations index, state index)
		 * @return initial value for delta
		 */
	private def initial(ti: (Int, Int)): Double = {
		if(ti._1 == 0) {
			state.psi += (0, 0, 0)
			state.delta(ti._1, ti._2) + lambda.pi(ti._2) * lambda.B(ti._1, obs(ti._2))
		}
		else 
			-1.0
	}

		/*
		 * Recursive computation of psi and delta [M12] and [M14] along the 
		 * observations O(t) t 0 -> T.
		 */
	private def recurse(t: Int, j: Int): Double = {
	  
			// Initialization of the delta value, return -1.0 in case of error
		var maxDelta = initial((t, j))
			
			// If this is not the initial state ..
		if( maxDelta == -1.0) {
			
			// .. and not the last observation ..
			if( t != obs.size) {
				// Compute the maximum value of delta at observation t
				// give the delta value at observation t-1, the transition probabilities A
			  // and the emission probabilities matrix B  [M14]
				maxDelta = maxBy(lambda.getN, s => 
					recurse(t-1, s)* lambda.A(s, j)* lambda.B(j, obs(t)) )
				
				// Compute the arg max {delta
				val idx = maxBy(lambda.getT, i =>recurse(t-1 ,i)*lambda.A(i,j))
				state.psi += (t, j, idx)
				state.delta += (t, j, maxDelta)
			}
			
			// in case of the last observation, compute psi that maximize
			// the product delta @ t-1 and A
			else {
				maxDelta = 0.0  		   
				val index = maxBy(lambda.getN, i => { 
					val delta = recurse(t-1 ,i)
						if( delta > maxDelta) 
							maxDelta = delta
						delta
				})
				
				// Update the Q* value with the index that maximize the delta.A
				state.QStar.update(t, index)
			}
		}
		maxDelta
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
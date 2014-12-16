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
import HMMConfig._


		/**
		 * <p>Class that implements the Viterbi algorithm to extract the best sequence
		 * of hidden states in a HMM given a lambda model and a sequence of integer
		 * observations. The maximum value of delta is computed recursively during 
		 * instantiation.</p>
		 *  @constructor Create an instance of the Viterbi algorithm for a predefined Lambda model 
		 *  and set of observations.
		 *  @throws IllegalArgumentException if lambda, params and observations are undefined of 
		 *  eps is out of range
		 *  @see org.scalaml.hmm.HMMModel
		 *  @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 *  probabilities, the state-transition probabilities matrix and the emission proabilities matrix.
		 *  @param obs Array of observations as integer (categorical data)
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

	private def initial(ti: (Int, Int)): Double = {
		if(ti._1 == 0) {
			state.psi += (0, 0, 0)
			state.delta(ti._1, ti._2) + lambda.pi(ti._2) * lambda.B(ti._1, obs(ti._2))
		}
		else 
			-1.0
	}
   
	private def recurse(t: Int, j: Int): Double = {
	  
			// Initialization of the delta value, return -1.0 in case of error
		var maxDelta = initial((t, j))
		if( maxDelta == -1.0) {
			if( t != obs.size) {
				maxDelta = maxBy(lambda.getN, s => 
					recurse(t-1, s)* lambda.A(s, j)* lambda.B(j, obs(t)) )
					
				val idx = maxBy(lambda.getT, i =>recurse(t-1 ,i)*lambda.A(i,j))
				state.psi += (t, j, idx)
				state.delta += (t, j, maxDelta)
			}
			else {
				maxDelta = 0.0  		   
				val index = maxBy(lambda.getN, i => { 
					val delta = recurse(t-1 ,i)
						if( delta > maxDelta) 
							maxDelta = delta
						delta
				})
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
		 * probabilities, the state-transition probabilities matrix and the emission proabilities matrix.
		 * @param obs Array of observations as integer (categorical data)
		 */
	def apply(lambda: HMMLambda, _labels: Array[Int]): ViterbiPath = 
			new ViterbiPath(lambda, _labels)
}


// -------------------------  EOF ----------------------------------------------
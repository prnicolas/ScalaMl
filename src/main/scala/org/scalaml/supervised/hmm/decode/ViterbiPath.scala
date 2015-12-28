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
package org.scalaml.supervised.hmm.decode

import scala.annotation.tailrec

import org.scalaml.util.MathUtils._
import org.scalaml.supervised.hmm.{HMMModel, HMMPrediction}

		/**
		 * Class that implements the Viterbi algorithm to extract the best sequence
		 * of hidden states in a HMM given a lambda model and a sequence of observations. The 
		 * maximum value of delta is computed recursively during instantiation
		 * 
		 * @constructor Create an instance of the Viterbi algorithm for a predefined Lambda model 
		 *  and set of observations.
		 * @see org.scalaml.hmm.HMMModel
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 *  probabilities, the state-transition probabilities matrix and the emission probabilities 
		 *  matrix.
		 * @param obsSeq Array of observations as integer (categorical data)
		 *  
		 * @author Patrick Nicolas
		 * @since 0.98 January 16, 2015
		 * @version 0.99.1
		 * @see Scala for Machine Learning Chapter 7 ''Sequential data models'' / Hidden Markov Model 
		 *  / Decoding
		 */
final protected class ViterbiPath(lambda: HMMModel, obsSeq: Vector[Int]) {
	private val numStates = lambda.numStates
	private val numObs = lambda.numObs
  	
		/**
		 * Auxiliary matrix of indices that maximize the probability of a given sequence of states
		 */
	private val psi = Array.fill(numObs)(Array.fill(numStates)(0))
	
	private val qStar = new QStar(numObs, numStates)
	
		/**
		 * Matrix of elements (t, i) that defines the highest probability of a single path of 
		 * t observations reaching state S(i). It Compute the delta value given the 
		 * initial probabilities pi and the first observed data, obs( t = 0).
		 * @return initial value for maximum likelihood (delta) for the first observation t = 0
		 */
	private val delta = 
		Range(0, numStates)./:(DMatrix(numObs, numStates))((m, n)
			=> {
			psi(0)(n) = 0
			m += (0, n, lambda.pi(n) * lambda.B(n, obsSeq.head))
		})

		/**
		 * Pair (prob, states_sequence) where states_sequence is the
		 * the most likely sequence of states given a Lambda model and a sequence
		 * of observations, and prob its likelihood. The states of sequence uses a tail recursion.
		 * The method uses the notation introduced in "Scala for Machine Learning"
		 * and "Introduction to Machine Learning" by E. Alpaydin.
		 */
	val path: HMMPrediction = {
	  
		@tailrec
		def viterbi(t: Int): Double = {
	
				// Update the maximum delta value and its state index for the observation t
			Range(0, numStates).foreach( updateMaxDelta(t, _) )
				
				// If we reached the last observation... exit by backtracing the
				// computation of the 
			if( t ==  obsSeq.size-1) {
				val idxMaxDelta = Range(0, numStates).map(i => (i, delta(t, i))).maxBy(_._2)
						
				// Update the Q* value with the index that maximize the delta.A
				qStar.update(t+1, idxMaxDelta._1, psi)
				idxMaxDelta._2
			}
			else 
				viterbi(t+1)
		}
	  HMMPrediction(viterbi(1), qStar())
	}


	
		/*
		 * Update the value of the index psi of the maximum likelihood (maximum delta)
		 * for the observation of index t and to state j
		 */
	private def updateMaxDelta(t: Int, j: Int): Unit = {
			// compute the maximum delta for the states for this observation and
			// the its state index.
		val idxDelta = Range(0, lambda.numStates)
				.map(i => (i, delta(t-1, i)*lambda.A(i, j))).maxBy(_._2)
				
		psi(t)(j) = idxDelta._1
		delta += (t, j, idxDelta._2)
	}
}

	/**
	 * Object companion for the Viterbi algorithm for the extraction of best sequences. Implements the constructor - apply
	 * @author Patrick Nicolas
	 * @since 0.98 March 17, 2014
	 * @version 0.98.2
	 */
object ViterbiPath {
		/**
		 * Default constructor for the Viterbi algorithm
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 * probabilities, the state-transition probabilities matrix and the emission probabilities 
		 * matrix.
		 * @param obsSeq Array of observations as integer (categorical data)
		 */
	def apply(lambda: HMMModel, obsSeq: Vector[Int]): ViterbiPath = 
			new ViterbiPath(lambda, obsSeq)
}


// -------------------------  EOF ----------------------------------------------
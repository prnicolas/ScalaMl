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
package org.scalaml.supervised.hmm.train

import scala.util.Try

import org.scalaml.util.MathUtils._
import org.scalaml.supervised.hmm.HMMConfig
import HMMConfig._

		/**
		 * Class that encapsulates the computation and update of the joint probability
		 * matrix DiGamma(t, i ,j) of to be within a state S(i),  transition to state S(j) at 
		 * observation of index t [Formula M8].
		 * 
		 * Di-Gamma is an array of T-1 states matrices (for T observations)
		 * @constructor Create an array of matrices DiGamma structure
		 * @param numObs Number of observations
		 * @param numStates Number of states
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98.1 March 24, 2014
		 * @version 0.99.1
		 * @see Scala for Machine Learning Chapter 7 ''Sequential Data Models'' / Hidden Markov Model 
		 * - Training / Baum-Welch
		 */
final class DiGamma(numObs: Int, numStates: Int) {
		private val diGamma = Array.fill(numObs-1)(DMatrix(numStates))
		
			/**
			 * Update the value of the diGamma, given a sequence of observations (indices), the
			 * state transition probabilities matrix A, the emission probability matrix B, the 
			 * the probability alpha of being in state S(j), given a sequence of observations 
			 * [0, t] and the probability beta of being in the same state S(j) given the sequence of 
			 * observations [t+1, T-1].
			 * @param alpha Matrix of the probabilities of being in the state given a sequence of
			 * observations [0, t]
			 * @param beta Matrix of the probabilities of being in the state given a sequence of
			 * observations [t+1, T-1]
			 * @param A Matrix of state transition probabilities
			 * @param B Matrix of emission probabilities
			 * @param obsSeq Indices of observations [0, T-1]
			 * @return Number of observations if the update succeeds, -1 otherwise.
			 */
		def update(
				alpha: DMatrix, 
				beta: DMatrix, 
				A: DMatrix, 
				B: DMatrix, 
				obsSeq: Vector[Int]): Try[Int] = Try {
		  
					// walk through all the observations of indices 0 -> T-1
					// to compute the Di-gamma matrix for each observations
			foreach(numObs-1, t => {
				  
					// Computes elements of the diGamma matrix for this observation t 
					// and the sum of the diGamma for all these states (Numerator of
					// of formula M10)
				val sum =  /:(numStates, (sst, i) => {
					sst + /:(numStates, (s, j) => {
					diGamma(t) += (i, j, alpha(t,i)*beta(t+1, i)* A(i,j)*B(j, obsSeq(t+1)))
						s + diGamma(t)(i, j)
					})
				})

					// Normalize the Di gamma values for this observation t
				foreach(numStates, i => 
					foreach(numStates, j => diGamma(t) += (i, j,  diGamma(t)(i,j)/sum) ) )
			})
			obsSeq.size
		}
		
			/**
			 * Fold operator for the summation of the joint probability of being
			 * in state S(i) and transition to state S(j) at observation t
			 * @param t Index in the sequence of observations
			 * @param i Index of this state
			 * @param j Index of the state to transition to
			 */
		def fold(t: Int, i: Int, j: Int): Double = /:(t, (s, k) => s + diGamma(k)(i,j) )
	}



// ----------------------------------------  EOF ------------------------------------------------------------
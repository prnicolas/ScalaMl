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
import scala.reflect.ClassTag
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.DisplayUtils
import HMMConfig._

		/**
		 * <p>Class that encapsulates the execution parameters for the three
		 * canonical forms of the HMM. The variables are:<ul>
		 * <li><b>alpha(t, i)</b>: Forward probability or relevant probability for state S(i) given 
		 * a sequence of observation {0, 1,   t} [Formula M3]</li>
		 * <li><b>beta(t, i)</b>: Backward probability or relevant probability for state S(i) given
		 * the observations {t+1, t+2, ... T-1}</li>
		 * <li><b>delta(t, i)</b>: Highest probability of a single path of t observations 
		 * which ends with a given state S(i) (Viterbi algorithm)</li>
		 * <li><b>gamma(t, i)</b>: Probability of being in a given state S(i) at observation t
		 * [Formula M9]</li>
		 * <li><b>Di-gamma(t, i, j)</b>: The joint probability to be within a state S(i), transition
		 * to state S(j) at observation t + 1 [Formula M8]</li>
		 * <li><b>Psi(t, i)</b>: Auxiliary variable that computes the index of the state that
		 * maximum the probability of a single path of a sequence of t observations.[Formula M14]</li>
		 * </ul>
		 * @constructor Create a new execution state for the HMM for a predefined Lambda model
		 * @see "A Revealing Introduction to Hidden Markov Models" http://www.cs.sjsu.edu/~stamp/RUA/HMM.pdf
		 * for notation.
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 * probabilities, the state-transition probabilities matrix and the emission probabilities matrix.
		 * @param maxIters   Maximum number of iterations used in training (Baum-Welch)
		 * 
		 * @author Patrick Nicolas
		 * @since March 24, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model
		 */
final protected class HMMState(val lambda: HMMLambda, val maxIters: Int) {
	import HMMState._
  
	private val logger = Logger.getLogger("HMMState")

		/**
		 * Matrix of elements (t, i) that defines the highest probability of a single path of 
		 * t observations reaching state S(i)
		 */
	val delta = Matrix[Double](lambda.getT, lambda.getN)
	
		/**
		 * Auxiliary matrix of indices that maximize the probability of a given sequence of states
		 */
	val psi = Matrix[Int](lambda.getT, lambda.getN)

		/**
		 * Singleton to compute the sequence Q* of states with the highest probability given 
		 * a sequence of observations.
		 */
	object QStar {
		private val qStar = Array.fill(lambda.getT)(0)

			/**
			 * Update Q* the optimum sequence of state
			 * @param t the index in the sequence of observation 
			 * @param index index of the state
			 */
		def update(t: Int, index: Int): Unit = {
			qStar(t-1) = index
			(t-2 to 0 by -1).foreach( s => {qStar(s) = psi(s+1, qStar(s+1)) })
		}

			/**
			 * Access the sequence of states with the highest probability
			 */
		def apply(): Array[Int] = qStar
	}

		/**
		 * Singleton that encapsulates the computation and update of the joint probability
		 * matrix DiGamma(t, i ,j) of to be within a state S(i),  transition to state S(j) at 
		 * observation of index t [Formula M8].
		 * Di-Gamma is an array of T-1 states matrices (for T observations)
		 * @see Chapter 7 Sequential data models / Hidden Markov Model / Training / Baum-Welch
		 */
	object DiGamma {
		private val diGamma = Array.fill(lambda.getT-1)(Matrix[Double](lambda.getN, lambda.getN))
		
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
			 * @param obs Indices of observations [0, T-1]
			 * @return Number of observations if the update succeeds, -1 otherwise.
			 */
		def update(
				alpha: Matrix[Double], 
				beta: Matrix[Double], 
				A: Matrix[Double], 
				B: Matrix[Double], 
				obs: Array[Int]): Int = {
		  
			Try {
					// walk through all the observations of indices 0 -> T-1
					// to compute the Di-gamma matrix for each observations
				foreach(lambda.getT-1, t => {
				  
					// Computes elements of the diGamma matrix for this observation t 
					// and the sum of the diGamma for all these states (Numerator of
					// of formula M10)
					val sum =  foldLeft(lambda.getN, (sst, i) => {
						sst + foldLeft(lambda.getN, (s, j) => {
							diGamma(t) += (i, j, alpha(t,i)*beta(t+1, i)* A(i,j)*B(j, obs(t+1)))
							s + diGamma(t)(i, j)
						})
					})

					// Normalize the Di gamma values for this observation t
					foreach(lambda.getN, i => 
						foreach(lambda.getN, j => diGamma(t) += (i, j,  diGamma(t)(i,j)/sum) ) )
				})
				obs.size
			} 
			match {
				case Success(n) => n
				case Failure(e) => DisplayUtils.error("HMMState.DiGamma", logger, e)
			}
		}
		
			/**
			 * Fold operator for the summation of the joint probability of being
			 * in state S(i) and transition to state S(j) at observation t
			 * @param t Index in the sequence of observations
			 * @param i Index of this state
			 * @param j Index of the state to transition to
			 */
		def fold(t: Int, i: Int, j: Int): Double = foldLeft(t, (s, k) => s + diGamma(k)(i,j) )
	}
  
		/**
		 * Matrix of the probability of being in a given state S(i) at observation of 
		 * index t [Formula M9]
		 * @see Chapter 7 Sequential data models / Hidden Markov Model / Training / Baum-Welch
		 */
	object Gamma {
		private val values = Matrix[Double](lambda.getT, lambda.getN)
  	
			/**
			 * Update the probabilities of being in a given state for all the observations
			 * @see Chapter 7 Sequential data models / Hidden Markov Model / Training / Baum-Welch 
			 * [Formula M8]
			 * @param alpha Alpha matrix of relevant probabilities of being in the  state S(i) 
			 * given a sequence of observation {0, 1, ... t}
			 * @param beta Beta matrix of probabilities of being in a state S(i) given the 
			 * observations {t+1, t+2, ... T-1}
			 */
		def update(alpha: Matrix[Double], beta: Matrix[Double]): Unit = {
			
			foreach(lambda.getT, t => {
				// Compute the denominator of formula M8
				val sum = foldLeft(lambda.getN, (s, i) => {
					values += (t, i, alpha(t,i)*beta(t,i))
					s + values(t,i) 
				})
				values.row(t).map( _ / sum)
			})
		}

			/**
			 * Fold operator for the summation of the probability of being in
			 * a state i give the observations {0, 1, ... t}
			 * @param t index of the observation in the sequence
			 * @param i index of the state S(i)
			 * @return the probability being in a given state for all the observations
			 */
		def fold(t: Int, i: Int): Double = foldLeft(t, (s, n) => s + values(n, i))
		
			/**
			 * Fold operator for the summation of the probability of being in
			 * a state i give the observations {0, 1, ... t}
			 * @param t index of the observation in the sequence
			 * @param i index of the state S(i)
			 * @return the probability being in a given state for all the observations
			 */
		def fold(t: Int, i: Int, k: Int, obs: Array[Int]): Double = 
				foldLeft(t, (s, n) => s + { if(obs(n) ==k) values(n, i) else 0.0} )
 
			/**
			 * Retrieve the gamma value (probability) for an observation at 
			 * index t, (t+1 nth observation) for a state of index j S(j)
			 * @param t Index of the observation in the sequence
			 * @param j index of the state
			 */
		def apply(t: Int, j: Int): Double = values(t,j)
	}
	
	 
		/**
		 * Method to update the Gamma and Di_Gamma values (Baum-Welch estimator). The 
		 * actual computation is delegated to the inner singleton Gamma and DiGamma.
		 * @param alpha Matrix of the probabilities of being in the state given a sequence of
		 * observations [0, t]
		 * @param beta Matrix of the probabilities of being in the state given a sequence of
		 * observations [t+1, T-1]
		 * @param A Matrix of state transition probabilities
		 * @param B Matrix of emission probabilities
		 * @param obs Indices of observations [0, T-1]
		 */	 
	def update(
			alpha: Matrix[Double], 
			beta: Matrix[Double], 
			A: Matrix[Double], 
			B: Matrix[Double], 
			obs: Array[Int]): Int= {
	  
			// Update Gamma(t, i)
		Gamma.update(alpha, beta)
			// Update DiGamma(t, i, j)
		DiGamma.update(alpha, beta, A, B, obs)
	}
}

		/**
		 * Companion object for the HMM state. This singleton is used to define
		 * the constructors of the HMMState class
		 * @author Patrick Nicolas
		 * @since March 24, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models/Hidden Markov Model
		 */
object HMMState {
	val DEFAULT_MAXITERS = 256
	
		/**
		 * Default constructor for the HMMState class
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 * probabilities, the state-transition probabilities matrix and the emission proabilities matrix.
		 * @param >maxIters   Maximum number of iterations used in training (Baum-Welch)
		 */
	def apply(lambda: HMMLambda, maxIters: Int): HMMState = new HMMState(lambda, maxIters)
	
		/**
		 * Constructor for the HMMState class with a predefined maximum number of iterations 
		 * used in Baum-Welch algorithm
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 * probabilities, the state-transition probabilities matrix and the emission proabilities matrix.
		 */
	def apply(lambda: HMMLambda): HMMState = new HMMState(lambda, DEFAULT_MAXITERS)
}

// ----------------------------------------  EOF ------------------------------------------------------------
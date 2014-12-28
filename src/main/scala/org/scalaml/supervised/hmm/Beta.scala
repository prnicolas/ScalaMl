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

import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger

import org.scalaml.core.Matrix
import org.scalaml.util.DisplayUtils
import HMMConfig._

		/**
		 * <p>Implementation of the Beta or backward pass of the 
		 * HMM algorithm to compute the probability of a sequence of observations. The beta
		 * matrix is computed as part of the instantiation of the class. It measures the probability 
		 * of being in state S(i) given the observations {t+1, t+2, ... T-1}</p>
		 * @constructor Create a Beta (or backward) pass for the 1st canonical form of HMM
		 * @throws IllegalArgumentException if lambda model or the observations are undefined
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 * probabilities, the state-transition probabilities matrix and the emission probabilities matrix.
		 * @param obs Array of observations as integer (categorical data)
		 * @see Chapter 7 Sequential Data Models / Hidden Markov model / Evaluation / Beta pass
		 * @see org.scalaml.supervised.hmm.Pass
		 * 
		 * @author Patrick Nicolas
		 * @since March 14, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model / 
		 * Evaluation
		 */
protected class Beta(lambda: HMMLambda, obs: Array[Int]) extends Pass(lambda, obs) {
	private val logger = Logger.getLogger("Beta")

			/**
			 * Initializes the Beta values (alphaBeta is used as alpha for the Alpha pass and 
			 * beta for the Beta pass). The initialization implements the formula M7 which
			 * computes the beta value at observation t as the summation of the Beta values at
			 * observation t+1 multiplied by the transition probability aij and the emission 
			 * probabilities bj for the observation at t+1
			 * @see Chapter 7 Sequential Data Models / Hidden Markov model / Evaluation / Beta pass
			 */
	val complete = {
		Try {
				// Creates the matrix of probabilities of a state given the 
				// observations, and initialize the probability for the last observation 
				// (index T-1) as 1.0
			alphaBeta = Matrix[Double](lambda.getT, lambda.getN)	
			alphaBeta += (lambda.d_1, 1.0)
				// Normalize by computing (ct)
			normalize(lambda.d_1)
				// Compute the beta probabilites for all the observations.
			sumUp
		} 
		match {
			case Success(t) => true
			case Failure(e) => DisplayUtils.error("Beta.complete failed", logger, e); false
		}
	}
	
		/*
		 * Update the beta values from the observations T-1 to the first observations
		 * (index: 0). THe value is then normalized, c(t)
		 * @see Chapter 7 Sequential Data Models / Hidden Markov model / Evaluation / Alpha pass
		 */
	private def sumUp: Unit = {
			// Update and normalize the beta probabilities for all 
			// the observations starting with index T-2.. befor normalization.
		(lambda.getT-2 to 0 by -1).foreach( t =>{
			updateBeta(t)
			normalize(t) 
		})
	}
	
		/*
		 * Implements the update of beta(t) from beta(t+1) for all the states using 
		 * the transition probabilities A and the emission matrix B
		 */
	private def updateBeta(t: Int): Unit =
		foreach(lambda.getN, i =>
				alphaBeta += (t, i, lambda.beta(alphaBeta(t+1, i), i, obs(t+1))))
}


		/**
		 * Companion object for the Beta pass that defines the constructor apply
		 * @author Patrick Nicolas
		 * @since March 14, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model / 
		 * Evaluation
		 */
object Beta {
		/**
		 * Default constructor for the Beta class of forward/backward passes in HMM
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state probabilities, the state-transition probabilities matrix and the emission proabilities matrix.
		 * @param obs Array of observations as integer (categorical data)
		 */
	def apply(lambda: HMMLambda,  obs: Array[Int]): Beta = new Beta(lambda, obs)
}


// --------------------------------  EOF -------------------------------------
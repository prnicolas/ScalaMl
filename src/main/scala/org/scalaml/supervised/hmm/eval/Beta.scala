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
 * Version 0.99
 */
package org.scalaml.supervised.hmm.eval

import scala.util.Try
import org.apache.log4j.Logger

import org.scalaml.util.MathUtils._
import org.scalaml.util.LoggingUtils._
import org.scalaml.supervised.hmm.{HMMModel, HMMConfig, HMMTreillis}
import HMMConfig._

		/**
		 * Implementation of the Beta or backward pass of the 
		 * HMM algorithm to compute the probability of a sequence of observations. The beta
		 * matrix is computed as part of the instantiation of the class. It measures the probability 
		 * of being in state S(i) given the observations {t+1, t+2, ... T-1}
		 * @constructor Create a Beta (or backward) pass for the 1st canonical form of HMM
		 * @throws IllegalArgumentException if lambda model or the observations are undefined
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 * probabilities, the state-transition probabilities matrix and the emission probabilities 
		 * matrix.
		 * @param obs Array of observations as integer (categorical data)
		 * @see Chapter 7 Sequential Data Models / Hidden Markov model / Evaluation / Beta pass
		 * @see org.scalaml.supervised.hmm.Pass
		 * 
		 * @author Patrick Nicolas
		 * @since March 14, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model / 
		 * Evaluation
		 */
protected class Beta(lambda: HMMModel, obsSeq: Vector[Int]) 
     extends HMMTreillis(lambda.numObs, lambda.numStates) {
	private val logger = Logger.getLogger("Beta")

			/**
			 * Initializes the Beta values (alphaBeta is used as alpha for the Alpha pass and 
			 * beta for the Beta pass). The initialization implements the formula M7 which
			 * computes the beta value at observation t as the summation of the Beta values at
			 * observation t+1 multiplied by the transition probability aij and the emission 
			 * probabilities bj for the observation at t+1
			 * @see Chapter 7 Sequential Data Models / Hidden Markov model / Evaluation / Beta pass
			 */
	val initialized: Boolean = Try {
				// Creates the matrix of probabilities of a state given the 
				// observations, and initialize the probability for the last observation 
				// (index T-1) as 1.0
		treillis = DMatrix(lambda.numObs, lambda.numStates)	
		treillis += (lambda.numObs-1, 1.0)
				// Normalize by computing (ct)
		normalize(lambda.numObs-1)
				// Compute the beta probabilites for all the observations.
		sumUp
	}._toBoolean("Beta.complete failed")
	
	@inline
	override def isInitialized: Boolean = initialized
	
		/*
		 * Update the beta values from the observations T-1 to the first observations
		 * (index: 0). THe value is then normalized, c(t)
		 * @see Chapter 7 Sequential Data Models / Hidden Markov model / Evaluation / Alpha pass
		 */
	private def sumUp: Unit = 
			// Update and normalize the beta probabilities for all 
			// the observations starting with index T-2.. befor normalization.
		(lambda.numObs-2 to 0 by -1).foreach( t =>{
			updateBeta(t)
			normalize(t) 
		})
	
	
		/*
		 * Implements the update of beta(t) from beta(t+1) for all the states using 
		 * the transition probabilities A and the emission matrix B
		 */
	private def updateBeta(t: Int): Unit =
		foreach(lambda.numStates, i =>
				treillis += (t, i, lambda.getBetaVal(treillis(t+1, i), i, obsSeq(t+1))))
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
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 * probabilities, the state-transition probabilities matrix and the emission proabilities 
		 * matrix.
		 * @param obs Array of observations as integer (categorical data)
		 */
	def apply(lambda: HMMModel,  obsSeq: Vector[Int]): Beta = new Beta(lambda, obsSeq)
}


// --------------------------------  EOF -------------------------------------
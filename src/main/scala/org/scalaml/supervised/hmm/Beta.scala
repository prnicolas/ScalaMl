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

import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger

import org.scalaml.core.Matrix
import org.scalaml.util.DisplayUtils
import HMMConfig._

		/**
		 * <p>Implementation of the Beta or backward pass of the 
		 * HMM algorithm to compute the probability of a sequence of observations. The beta
		 * matrix is computed as part of the instantiation of the class.</p>
		 * @constructor Create a Beta (or backward) pass for the 1st canonical form of HMM
		 * @throws IllegalArgumentException if lambda model or the observations are undefined
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 * probabilities, the state-transition probabilities matrix and the emission proabilities matrix.
		 * @param obs Array of observations as integer (categorical data)
		 * @author Patrick Nicolas
		 * @since March 14, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model / 
		 * Evaluation
		 */
protected class Beta(lambda: HMMLambda, obs: Array[Int]) extends Pass(lambda, obs) {
	private val logger = Logger.getLogger("Beta")

	val complete = {
		Try {
			alphaBeta = Matrix[Double](lambda.getT, lambda.getN)	
			alphaBeta += (lambda.d_1, 1.0)
			normalize(lambda.d_1)
			sumUp
			true
		} match {
			case Success(t) => t
			case Failure(e) => DisplayUtils.error("Failed beta computation ", logger, e); false
		}
	}
	
	private def sumUp: Unit = 
		(lambda.getT-2 to 0 by -1).foreach( t =>{
			updateBeta(t)
			normalize(t) 
		})
	  	
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
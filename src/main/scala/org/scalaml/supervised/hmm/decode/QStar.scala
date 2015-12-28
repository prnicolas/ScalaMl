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

		/**
		 * Class that encapsulates the execution parameters for Viterbi algorithm
		 * @constructor Create a QStar structure to collect the Viterbi optimum sequence of states
		 * @param numObs Number of observations
		 * @param numStates Number of states
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98.2 March 24, 2014
		 * @see Scala for Machine Learning Chapter 7 ''Sequential Data Models'' / Hidden Markov Model
		 * @see org.scalaml.supervised.hmm.decode.ViterbiPath
		 */

private[scalaml] final class QStar(numObs: Int, numStates: Int) {
	private val qStar = Array.fill(numObs)(0)		

		/**
		 * Update Q* the optimum sequence of state using backtracking..
		 * @param t the index in the sequence of observation 
		 * @param index index of the state
		 * @param psi matrix of indices 
		 */
	def update(t: Int, index: Int, psi: Array[Array[Int]]): Unit = {
		qStar(t-1) = index
		(t-2 to 0 by -1).foreach( s => {qStar(s) = psi(s+1)(qStar(s+1)) })
	}

			/**
			 * Access the sequence of states with the highest probability
			 */
	def apply(): Array[Int] = qStar
}

	

// ----------------------------------------  EOF ------------------------------------------------------------
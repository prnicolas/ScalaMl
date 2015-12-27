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
package org.scalaml.supervised.hmm

import scala.util.{Try, Success, Failure}
import scala.annotation.switch

import org.apache.log4j.Logger

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.Design.Model
import org.scalaml.core.ETransform
import org.scalaml.util.LoggingUtils._
import org.scalaml.util.MathUtils._
import HMM._



	/**
	 * Generic class for the alpha (forward) pass and beta (backward) passes used in
	 * the evaluation form of the HMM.
	 * @constructor Create an array of matrices DiGamma structure
	 * @param numObs Number of observations
	 * @param numStates Number of states
	 * 
	 * @author Patrick Nicolas
	 * @since 0.98.1 March 29, 2014
	 * @version 0.99.1
	 * @see Scala for Machine Learning Chapter 7 ''Sequential Data Models'' /Hidden Markov Model
	 */
protected abstract class HMMTreillis(numObs: Int, numStates: Int){ 
	protected var treillis: DMatrix = _
	protected val ct = Array.fill(numObs)(0.0)

		/**
		 * Compute and apply the normalization factor ct for the computation of Alpha
		 * [Formula M3] and Beta probabilities [Formula M7] for the observation at index t
		 * @param t Index of the observation.
		 */
	protected def normalize(t: Int): Unit = {
		import HMMConfig._
		require(t >= 0 && t < numObs, s"HMMModel.normalize Incorrect observation index t= $t")
		
		ct.update(t, /:(numStates, (s, n) => s + treillis(t, n)))
		treillis /= (t, ct(t))
	}

	@inline
	def isInitialized: Boolean
	
		/**
		 * Returns the Alpha (resp. Beta) matrix for the forward (resp. backward) matrix
		 * @return Alpha or Beta matrix
		 */
	@inline
	final def getTreillis: DMatrix = treillis
}


// ----------------------------------------  EOF ------------------------------------------------------------
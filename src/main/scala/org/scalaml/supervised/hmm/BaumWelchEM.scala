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
 * 
 */
package org.scalaml.supervised.hmm

import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.DisplayUtils
	
		/**
		 * <p>Class that update the backward-forward lattice of observations and
		 * hidden states for HMM using the Baum-Welch algorithm. The algorithm is used to 
		 * compute the likelihood of the conditional probability p(Y|X) during training. The
		 * computation is performed as part of the instantiation of the class (type Option[Double] )</p>
		 *  @constructor Create a new Baum-Welch expectation maximization instance to train a model 
		 *  given a set of observations.
		 *  @throws IllegalArgumentException if lambda, params and observations are undefined or 
		 *  eps is out of range
		 *  @param config Configuration parameters class instance for the HMM
		 *  @param obs Observations defined as an array of Integer (or categorical data)
		 *  @param numIters	Number of iterations allowed in the Baum-Welch EM optimization
		 *  @param eps Convergence criteria for the exit of the Baum-Welch EM.
		 *  @see org.scalaml.supervised.hmm.HMMModel
		 *  
		 *  @author Patrick Nicolas
		 *  @since March 15, 2014
		 *  @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model / 
		 *  Training
		 */
final protected class BaumWelchEM(
		config: HMMConfig, 
		obs: Array[Int], 
		numIters: Int, 
		eps: Double)	extends HMMModel(HMMLambda(config), obs) {
	import BaumWelchEM._
	
	check(config, obs, numIters, eps)
	private val logger = Logger.getLogger("BaumWelchEM")

		/**
		 * Initial state of the HMM model for training
		 */
	val state = HMMState(lambda, numIters)
  
		/**
		 * Maximum likelihood (maximum log of the conditional probability) extracted from the training
		 * @see Chapter 7 Sequential data models / Hidden Markov models / Training (CF-2)
		 */
	val maxLikelihood: Option[Double] = {
		Try {
			var likelihood = frwrdBckwrdLattice
		
				// Apply the alpha / beta algorithm for each state
				// until the likelihood value Alpha.alpha converges toward a maximum
			Range(0, state.maxIters) find( _ => {
				lambda.estimate(state, obs)
				val _likelihood = frwrdBckwrdLattice
				val diff = likelihood - _likelihood
				likelihood = _likelihood
		  	  
				diff < eps
			}) 
			match {
				case Some(index) => likelihood
				case None => throw new IllegalStateException("BaumWelchEM.maxLikelihood failed")
			}
		} 
		match {
				// If the maximum likelihood is computed, normalize the resulting Lambda model
			case Success(likelihood) => {
				state.lambda.normalize
				Some(likelihood)
			}
			case Failure(e) => DisplayUtils.none("BaumWelchEM.maxLikelihood", logger, e)
		}
	}
   
		/*
		 * Apply the alpha-beta passes to compute the probability Gamma of being in a 
		 * given state for each observation [Formula M8], the probability DiGamma to being 
		 * in a given state and transition to another given state for each observation [Formula M8]
		 * then update the state transition probabilities matrix A [Formula 10], the emission 
		 * probabilities matrix B [Formula M11] and the initial probabilities PI [Formula M9]
		 */
	private def frwrdBckwrdLattice: Double  = {
			// Compute the forward pass given the sequence of observations obs
		val _alpha = Alpha(lambda, obs)
		
			// Compute the probabilities of a state given the 
		state.update(_alpha.getAlphaBeta, Beta(lambda, obs).getAlphaBeta, lambda.A, lambda.B, obs)
			// Finally returns the likelihood
		_alpha.alpha
	}
}



		/**
		 * Object companion for Baum_Welch algorithm that defines the constructor for BaumWelchEM
		 * and validate its input parameters
		 * @see org.scalaml.supervised.hmm.HMMModel
		 * @author Patrick Nicolas
		 * @since March 15, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model / 
		 * Training
		 */
object BaumWelchEM {
	private val EPS = 1e-3   

		/**
		 * Default constructor for the BaumWelchEM class
		 *  @param config Configuration parameters class instance for the HMM
		 *  @param obs Observations defined as an array of Integer (or categorical data)
		 *  @param numIters	Number of iterations allowed in the Baum-Welch EM optimization
		 *  @param eps Convergence criteria for the exit of the Baum-Welch EM.
		 */
	def apply(config: HMMConfig, labels: Array[Int], numIters: Int, eps: Double): BaumWelchEM = 
		new BaumWelchEM(config, labels, numIters,eps)
	
	private val EPS_LIMITS = (1e-8, 0.1)
	private val MAX_NUM_ITERS = 1024
	
	private def check(config: HMMConfig, obs: Array[Int], numIters: Int, eps: Double): Unit = {
		require( !obs.isEmpty, "BaumWelchEM.check Observations are undefined")
		require(numIters > 1 && numIters < MAX_NUM_ITERS, 
				s"BaumWelchEM.check Maximum number of iterations $numIters is out of range")
		require(eps > EPS_LIMITS._1 && eps < EPS_LIMITS._2, 
				s"BaumWelchEM.check Convergence criteria for HMM Baum_Welch $eps is out of range")
	}

}
// -----------------------------  EOF --------------------------------
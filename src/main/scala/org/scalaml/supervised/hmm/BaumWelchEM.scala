/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 * 
 */
package org.scalaml.supervised.hmm

import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display
	
		/**
		 * <p>Class that update the backward-forward lattice of observations and
		 * hidden states for HMM using the Baum-Welch algorithm. The algorithm is used to 
		 * compute the likelihood of the conditional probability p(Y|X) during training. The
		 * computation is performed as part of the instantiation of the class (type Option[Double] )<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
		 * <b>config</b>	Configuration for the HMM
		 * <b>obs</b>		Observations defined as an array of Integer (or categorical data)
		 * <b>numIters</b>	Number of iterations allowed in the Baum-Welch EM optimization
		 * <b>eps</b>		Convergence criteria for the exit of the Baum-Welch EM.
		 * </span></pre></p>
		 *  @constructor Create a new BaumWelch algorithm execution instance
		 *  @throws IllegalArgumentException if lambda, params and observations are undefined of eps is out of range
		 *  
		 *  @author Patrick Nicolas
		 *  @since March 15, 2014
		 *  @note Scala for Machine Learning Chapter 7 Sequential data models/Hidden Markov Model - Training
		 */
final protected class BaumWelchEM(config: HMMConfig, obs: Array[Int], numIters: Int, eps: Double) 
						extends HMMModel(HMMLambda(config), obs) {
	import BaumWelchEM._
	
	check(config, obs, numIters, eps)
	private val logger = Logger.getLogger("BaumWelchEM")

	val state = HMMState(lambda, numIters)
  
		/**
		 * Maximum likelihood (maximum log of the conditional probability) extracted from the training 
		 */
	val maxLikelihood: Option[Double] = {
		Try {
			var likelihood = frwrdBckwrdLattice
		  
			Range(0, state.maxIters) find( _ => {
				lambda.estimate(state, obs)
				val _likelihood = frwrdBckwrdLattice
				val diff = likelihood - _likelihood
				likelihood = _likelihood
		  	  
				diff < eps
			}) match {
				case Some(index) => likelihood
				case None => throw new IllegalStateException("BaumWelchEM.maxLikelihood failed")
			}
		} 
		match {
			case Success(likelihood) => {
				state.lambda.normalize
				Some(likelihood)
			}
			case Failure(e) => Display.none("BaumWelchEM.maxLikelihood", logger, e)
		}
	}
   

	private[this] def frwrdBckwrdLattice: Double  = {
		val alphaM = Alpha(lambda, obs)
		state.update(alphaM.getAlphaBeta, Beta(lambda, obs).getAlphaBeta, lambda.A, lambda.B, obs)
		alphaM.alpha
	}
}



		/**
		 * Object companion for Baum_Welch algorithm that defines the constructors for BaumWelchEM
		 * @author Patrick Nicolas
		 * @since March 15, 2014
		 */
object BaumWelchEM {
	final val EPS = 1e-3   
	
	def apply(config: HMMConfig, labels: Array[Int], numIters: Int, eps: Double): BaumWelchEM = 
		new BaumWelchEM(config, labels, numIters,eps)
	
	final val EPS_LIMITS = (1e-8, 0.1)
	final val MAX_NUM_ITERS = 1024
	
	private def check(config: HMMConfig, obs: Array[Int], numIters: Int, eps: Double): Unit = {
		require(config != null, "BaumWelchEM.check Configuration is undefined")
		require(obs != null && obs.size > 0, "BaumWelchEM.check Observations are undefined")
		require(numIters > 1 && numIters < MAX_NUM_ITERS, s"BaumWelchEM.check Maximum number of iterations $numIters is out of range")
		require(eps > EPS_LIMITS._1 && eps < EPS_LIMITS._2, s"BaumWelchEM.check Convergence criteria for HMM Baum_Welch $eps is out of range")
	}

}
// -----------------------------  EOF --------------------------------
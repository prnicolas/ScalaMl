/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95
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
	 * computation is performed as part of the instantiation of the class (type Option[Double] )
	 *  @param lambdaBW Lambda (pi, A, B) model for the HMM
	 *  @param _params parameters used in any of the three canonical form of the HMM
     *  @param _obs Array of observations as integer (categorical data)
     *  @throws IllegalArgumentException if lambda, params and observations are undefined of eps is out of range
     *  
	 *  @author Patrick Nicolas
	 *  @since March 15, 2014
	 *  @note Scala for Machine Learning Chapter 7 Sequential data models/Hidden Markov Model - Training
	 */
class BaumWelchEM(config: HMMConfig, 
				  _obs: Array[Int], 
				  numIters: Int,
				  eps: Double) extends HMMModel(HMMLambda(config), _obs) {
  require( eps > 1E-5 && eps < 0.1, s"Convergence criteria for HMM Baum_Welch $eps is out of range")
	  
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
		  	case None => throw new IllegalStateException("Likelihood failed")
		  }
  	  } match {
  	  	case Success(likelihood) => {
  	  	    state.lambda.normalize
  	  	    Some(likelihood)
  	  	}
  	  	case Failure(e) => Display.none("BaumWelchEM ", logger, e)
  	  }
	}
   
   private def frwrdBckwrdLattice: Double  = {
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
   def apply(config: HMMConfig, labels: Array[Int], numIters: Int, eps: Double): BaumWelchEM = new BaumWelchEM(config, labels, numIters,eps)

}
// -----------------------------  EOF --------------------------------
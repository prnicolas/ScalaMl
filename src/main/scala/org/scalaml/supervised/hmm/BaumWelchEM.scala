/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
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
	 * computation is performeda as part of the instantiation of the class (type Option[Double] )
	 *  @param lambdaBW Lambda (pi, A, B) model for the HMM
	 *  @param _params parameters used in any of the three canonical form of the HMM
     *  @param _obs Array of observations as integer (categorical data)
     *  @throws IllegalArgumentException if lambda, params and observations are undefined of eps is out of range
	 *  @author Patrick Nicolas
	 *  @since March 15, 2014
	 *  @note Scala for Machine Learning
	 */
class BaumWelchEM(	val lambdaBW: HMMLambda, 
					val configBW: HMMConfig, 
					val obsBW: Array[Int], 
					val eps: Double) extends HMMInference(lambdaBW, configBW, obsBW) {
	
  private val logger = Logger.getLogger("BaumWelchEM")
  require( eps > 1E-5 && eps < 0.1, "Convergence criteria for HMM Baum_Welch " + eps + " is out of range")

  	/**
  	 * Maximum likelihood (maximum log of the conditional probability) extracted from the training 
  	 */
  val maxLikelihood: Option[Double] = {
  	  Try {
		  var likelihood = frwrdBckwrdLattice
		  Range(0, config.maxIters) find( _ => {
		  	  lambda.estimate(config, labels)
		  	  val _likelihood = frwrdBckwrdLattice
		  	  val diff = likelihood - _likelihood
		  	  likelihood = _likelihood
		  	  diff < eps
		  }) match {
		  	case Some(index) => likelihood
		  	case None => throw new IllegalStateException("Likelihood failed")
		  }
  	  } match {
  	  	case Success(likelihood) => Some(likelihood)
  	  	case Failure(e) => Display.error("BaumWelchEM ", logger, e); None
  	  }
	}
   
   private def frwrdBckwrdLattice: Double  = {
       val alpha = Alpha(lambda, labels).alpha
	   Beta(lambda, labels)
	   config.Gamma
	   config.DiGamma.update(lambda.A, lambda.B, labels)
	   alpha
   }
}



	/**
	 * Object companion for Baum_Welch algorithm that defines the constructors for BaumWelchEM
	 * @author Patrick Nicolas
	 * @since March 15, 2014
	 */
object BaumWelchEM {
   final val EPS = 1e-3
   def apply(lambda: HMMLambda, params: HMMConfig, _labels: Array[Int], eps: Double) = new BaumWelchEM(lambda, params, _labels, eps)
   def apply(lambda: HMMLambda, params: HMMConfig, _labels: Array[Int])  = new BaumWelchEM(lambda, params, _labels, EPS)
}
// -----------------------------  EOF --------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.supervised.hmm


import org.scalaml.core.Types
import Types._
import org.scalaml.supervised.Supervised
import org.scalaml.workflow.PipeOperator
import org.scalaml.core.XTSeries
import HMM._
import org.scalaml.util.Matrix
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display
import org.scalaml.supervised.Model

	/**
	 * <p>Enumeration class to specify the canonical form of the HMM</p>
	 * @author Patrick Nicolas
	 * @since March 9, 2014
	 */
object HMMForm extends Enumeration {
  type HMMForm = Value
  val EVALUATION, DECODING = Value
}



class HMMModel(val lambda: HMMLambda, val labels: Array[Int]) extends Model {
   require(lambda != null, "Cannot execute dynammic algorithm with undefined HMM lambda model")
   require(labels != null && labels.size > 0, "Cannot execute dynammic algorithm  with undefined observations")
}


	/**
	 * <p>Generic class for the alpha (forward) pass and beta (backward) passes used in
	 * the evaluation form of the HMM.</p>
	 * @param lambda Lambda (pi, A, B) model for the HMM
	 * @param obs: Array of observations as integer (categorical data)
	 * @throws IllegalArgumentException if lambda, params and observations are undefined
	 * @author Patrick Nicolas
	 * @since March 29, 2014
	 */
class Pass(val lp: HMMLambda, val lbls: Array[Int]) extends HMMModel(lp, lbls) { 
   import Types.ScalaMl._
  
   protected var alphaBeta: Matrix[Double] = null
   protected val ct = Array.fill(lambda.dim._T)(0.0)

   protected def normalize(t: Int): Unit = {
  	  require(t >= 0 && t < lambda.dim._N, "Incorrect argument " + t + " for normalization")
  	  ct.update(t, HMMDim.foldLeft(lambda.dim._N, (s, n) => s + alphaBeta(t, n)))
  	  alphaBeta /= (t, ct(t))
   }
}



	/**
	 * <p>Generic class for the Dynamic Programming algorithms used in the Hidden 
	 * Markov Model. The sub-classes are the alpha (forward) pass, beta (backward) pass,
	 * Viterbi algorithm the compute the best sequence of hidden states and the Baum-Welch
	 * algorithm used in training a HMM.</p>
	 * @param lambda Lambda (pi, A, B) model for the HMM
	 * @param params parameters (alpha, beta, gamma, digamma, q) used in any of the three
	 * canonical form of the HMM
	 * @param labels: Array of observations as integer (categorical data)
	 * @throws IllegalArgumentException if lambda, params and observations are undefined
	 * @author Patrick Nicolas
	 * @since March 7, 2014
	 */
class HMMInference(val li: HMMLambda, val config: HMMConfig, val _labels: Array[Int]) 
                                                    extends HMMModel(li,  _labels) {
  require(config != null, "Cannot execute dynammic algorithm  with undefined HMM execution parameters")
}






import HMMForm._
class HMM[@specialized T <% Array[Int]](val lambda: HMMLambda, val form: HMMForm, val maxIters: Int)  
                           extends PipeOperator[T, HMMPredictor] {

	private val logger = Logger.getLogger("HMM")
	require(lambda != null, "Cannot execute a HMM with undefined lambda model")
	require(form != null, "Cannot execute a HMM with undefined canonical form")
	require( maxIters > 1 && maxIters < 1000, "Maximum number of iterations to train a HMM " + maxIters + " is out of bounds")
	
	protected val config = HMMConfig(lambda.dim, maxIters)
	
		/**
		 * <p>Classifier for the Hidden Markov Model. The pipe operator evaluates the 
		 * HMM if form == EVALUATION or decodes a HMM if form == DECODING for a given
		 * set of observations obs and a lambda model.</p>
		 * @param obs set of observation of type bounded by Array[Int]
		 * @return HMMPredictor instance if no computation error occurs, NONE otherwise
		 */
	def |> (obs: T): Option[HMMPredictor] = {
		require(obs != null, "Cannot perform an evaluaton or decoding of HMM with undefined observations")
		
		Try { 
		   form match {
		     case EVALUATION => evaluate(obs)
		     case DECODING => decode(obs)
		   } 
		} match {
			case Success(prediction) => Some(prediction)
			case Failure(e) => Display.error("HMM.|> ", logger, e); None
		}
	}

		/**
		 * <p>Train HMM with a set of observations to extract the Lambda model.</p>
		 * @param  obs set of observation of type bounded by Array[Int]
		 * @return maximum log likelihood if no arithmetic function occurs, None otherwise
		 * @throws IllegalArgumentException if the set of observations is not defined
		 * @throws RuntimeException for computation error such as divide by zero
		 */
	def train(obs: T): Option[Double] = {
		require(obs != null, "Cannot train a HMM with undefined observations")
	    BaumWelchEM(lambda, config, obs).maxLikelihood
	}
		
	def decode(obs: Array[Int]): HMMPredictor = (ViterbiPath(lambda, config, obs).maxDelta, config.QStar())
	
	def evaluate(obs: Array[Int]): HMMPredictor = (-Alpha(lambda, obs).logProb, obs)
}



	/**
	 * <p>Companion object for the HMM that defines a HMMPredictor type and the constructor 
	 * apply for the HMM.</p>
	 * @author Patrick Nicolas
	 * @since March 11, 2014
	 */
object HMM {
	type HMMPredictor = (Double, Array[Int])
	def apply[T <% Array[Int]](lambda: HMMLambda, form: HMMForm, maxIters: Int): HMM[T] =  new HMM[T](lambda, form, maxIters)
	def apply[T <% Array[Int]](lambda: HMMLambda, form: HMMForm): HMM[T] =  new HMM[T](lambda, form, HMMConfig.DEFAULT_MAXITERS)
}



// ----------------------------------------  EOF ------------------------------------------------------------
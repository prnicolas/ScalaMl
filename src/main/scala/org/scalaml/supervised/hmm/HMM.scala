/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95
 */
package org.scalaml.supervised.hmm


import org.scalaml.core.types.ScalaMl._
import org.scalaml.core.design.{PipeOperator, Model}
import org.scalaml.core.XTSeries
import HMM._
import org.scalaml.util.Matrix
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display
import HMMConfig._

	/**
	 * <p>Enumeration class to specify the canonical form of the HMM</p>
	 * @author Patrick Nicolas
	 * @since March 9, 2014
	 * @note Scala for Machine Learning Chapter 7 $Hidden Markov Model - Evaluation
	 */
object HMMForm extends Enumeration {
  type HMMForm = Value
  val EVALUATION, DECODING = Value
}



abstract class HMMModel(val lambda: HMMLambda, val obs: Array[Int]) extends Model {
   require(lambda != null, "Cannot execute dynammic algorithm with undefined HMM lambda model")
   require(obs != null && obs.size > 0, "Cannot execute dynammic algorithm  with undefined observations")
   
   val persists = "models/hmm"
}


	/**
	 * <p>Generic class for the alpha (forward) pass and beta (backward) passes used in
	 * the evaluation form of the HMM.</p>
	 * @param lambda Lambda (pi, A, B) model for the HMM
	 * @param obsIdx: Array of observations as integer (categorical data)
	 * 
	 * @author Patrick Nicolas
	 * @since March 29, 2014
	 * @note Scala for Machine Learning Chapter 7 $Hidden Markov Model - Evaluation
	 */
class Pass(val lambd: HMMLambda, val _obsIdx: Array[Int]) extends HMMModel(lambd, _obsIdx) { 
   protected var alphaBeta: Matrix[Double] = _
   protected val ct = Array.fill(lambda.getT)(0.0)

   protected def normalize(t: Int): Unit = {
  	  require(t >= 0 && t < lambda.getT, s"Incorrect argument $t for normalization")
  	  ct.update(t, foldLeft(lambda.getN, (s, n) => s + alphaBeta(t, n)))
  	  alphaBeta /= (t, ct(t))
   }
   
   def getAlphaBeta: Matrix[Double] = alphaBeta
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
	 * @note Scala for Machine Learning Chapter 7 $Hidden Markov Model
	 */
class HMMInference(val lmdb: HMMLambda, val state: HMMState, val _obs: Array[Int]) 
                                                    extends HMMModel(lmdb,  _obs) {
  require(state != null, "Cannot execute dynammic algorithm  with undefined HMM execution parameters")
}


import HMMForm._
		/**
		 * <p>Implementation of the Hidden Markov Model (HMM). The HMM classifier defines the
		 * three canonical forms (Decoding, training and evaluation).</p>
		 * @param lambda  lambda model generated through training or used as input for the evaluation and decoding phase
		 * @param form define the form (evaluation or decoding) used in the prediction of sequence
		 * @param maxIters  maximum number of iterations used in the Baum-Welch algorithm
		 * @throws IllegalArgumentException if the any of the class parameters is undefined
		 * 
		 * @author Patrick Nicolas
		 * @since March 23, 2014
		 * @note Scala for Machine Learning Chapter 7 $Hidden Markov Model
		 */
protected class HMM[@specialized T <% Array[Int]](lambda: HMMLambda, form: HMMForm, maxIters: Int)(implicit f: DblVector => T)  
                           extends PipeOperator[DblVector, HMMPredictor] {

	private val logger = Logger.getLogger("HMM")
	require(lambda != null, "Cannot execute a HMM with undefined lambda model")
	require(form != null, "Cannot execute a HMM with undefined canonical form")
	require(maxIters > 1 && maxIters < 1000, s"Maximum number of iterations to train a HMM $maxIters is out of bounds")
	
	private val state = HMMState(lambda, maxIters)
	
		/**
		 * <p>Classifier for the Hidden Markov Model. The pipe operator evaluates the 
		 * HMM if form == EVALUATION or decodes a HMM if form == DECODING for a given
		 * set of observations obs and a lambda model.</p>
		 * @param obs set of observation of type bounded by Array[Int]
		 * @return HMMPredictor instance if no computation error occurs, NONE otherwise
		 */
		
	override def |> : PartialFunction[DblVector, HMMPredictor] = {
		case obs: DblVector if(obs != null && obs.size > 1) => {
			Try { 
			   form match {
			     case EVALUATION => evaluate(obs)
			     case DECODING => decode(obs)
			   } 
			} match {
				case Success(prediction) =>prediction
				case Failure(e) => Display.error("HMM.|> ", logger, e); null
			}
	   }
	}
		/**
		 * <p>Implements the 3rd canonical form of the HMM</p>
		 * @param obsIdx given sequence of observations
		 * @return HMMPredictor predictor as a tuple of (likelihood, sequence (array) of observations indexes)
		 */
	def decode(obs: T): HMMPredictor = (ViterbiPath(lambda, obs).maxDelta, state.QStar())
	
		/**
		 * <p>Implements the 'Evaluation' canonical form of the HMM</p>
		 * @param obsIdx index of the observation O in a sequence
		 * @return HMMPredictor predictor as a tuple of (likelihood, sequence (array) of observations indexes)
		 */
	def evaluate(obs: T): HMMPredictor = (-Alpha(lambda, obs).logProb, obs)
	
	@inline
	final def getModel: HMMLambda = state.lambda
}



	/**
	 * <p>Companion object for the HMM that defines a HMMPredictor type and the constructor 
	 * apply for the HMM.</p>
	 * @author Patrick Nicolas
	 * @since March 11, 2014
	 */
object HMM {
		/**
		 * <p>Define the result of the prediction (decoding or evaluation) as a
		 * a tuple of (likelihood, sequence (array) of observations indexes).</p>
		 */
	type HMMPredictor = (Double, Array[Int])
	def apply[T <% Array[Int]](lambda: HMMLambda, form: HMMForm, maxIters: Int)(implicit f: DblVector => T): HMM[T] =  new HMM[T](lambda, form, maxIters)
	def apply[T <% Array[Int]](lambda: HMMLambda, form: HMMForm)(implicit f: DblVector => T): HMM[T] =  new HMM[T](lambda, form, HMMState.DEFAULT_MAXITERS)
	
	
    def apply[T <% Array[Int]](config: HMMConfig, obsIndx: Array[Int], form: HMMForm,  maxIters: Int, eps: Double)(implicit f: DblVector => T): Option[HMM[T]] = {
		val baumWelchEM = new BaumWelchEM(config, obsIndx, maxIters, eps)
		if( baumWelchEM.maxLikelihood != None)
		   Some(new HMM[T](baumWelchEM.lambda, form, maxIters))
		else None
	}
}



// ----------------------------------------  EOF ------------------------------------------------------------
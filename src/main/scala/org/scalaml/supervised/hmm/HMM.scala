/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.supervised.hmm

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.Design.{PipeOperator, Model}
import org.scalaml.core.XTSeries
import org.scalaml.core.Matrix
import scala.util.{Try, Success, Failure}
import scala.annotation.implicitNotFound
import org.apache.log4j.Logger
import org.scalaml.util.DisplayUtils
import HMM._

	/**
	 * <p>Enumeration class to specify the canonical form of the HMM</p>
	 * @author Patrick Nicolas
	 * @since March 9, 2014
	 * @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model
	 */
object HMMForm extends Enumeration {
	type HMMForm = Value
	val EVALUATION, DECODING = Value
}


import HMMForm._
		/**
		 * <p>Generic model for dynamic programming algorithms used in HMM.</p>
		 * @throws IllegalArgumenException If either Lambda or the observation are undefined.
		 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
		 * probabilities, the state-transition probabilities matrix and the emission probabilities matrix.
		 * @param obs Array of observations as integer (categorical data)
		 * @see org.scalaml.core.Design.Model
		 * 
		 * @author Patrick Nicolas
		 * @since March 29, 2014
		 * @note Scala for Machine Learning Chapter 7 / Sequential data models/Hidden Markov Model
		 */
abstract class HMMModel(val lambda: HMMLambda, val obs: Array[Int]) extends Model {
	import HMMModel._
	
	check(obs)
}


		/**
		 * Companion object for the HMM model parameters
		 */
object HMMModel {
	private def check(obs: Array[Int]): Unit = 
		require(!obs.isEmpty, "HMMModel.check Cannot create a model with undefined observations")
}


	/**
	 * <p>Generic class for the alpha (forward) pass and beta (backward) passes used in
	 * the evaluation form of the HMM.</p>
	 * @param lambda Lambda (pi, A, B) model for the HMM composed of the initial state 
	 * probabilities, the state-transition probabilities matrix and the emission probabilities matrix.
	 * @param obs Array of observations as integer (categorical data)
	 * 
	 * @author Patrick Nicolas
	 * @since March 29, 2014
	 * @note Scala for Machine Learning Chapter 7/Sequential data models/Hidden Markov Model
	 */
protected class Pass(lambda: HMMLambda, obs: Array[Int]) extends HMMModel(lambda, obs) { 
	protected var alphaBeta: Matrix[Double] = _
	protected val ct = Array.fill(lambda.getT)(0.0)

		/**
		 * Compute and apply the normalization factor ct for the computation of Alpha
		 * [Formula M3] and Beta probabilities [Formula M7] for the observation at index t
		 * @param t Index of the observation.
		 */
	protected def normalize(t: Int): Unit = {
		import HMMConfig._
		require(t >= 0 && t < lambda.getT, s"HMMModel.normalize Incorrect observation index t= $t")
		
		ct.update(t, foldLeft(lambda.getN, (s, n) => s + alphaBeta(t, n)))
		alphaBeta /= (t, ct(t))
	}

	def getAlphaBeta: Matrix[Double] = alphaBeta
}
		/**
		 * <p>Implementation of the Hidden Markov Model (HMM). The HMM classifier defines the
		 * three canonical forms (Decoding, training and evaluation).<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;
		 * ,Arial,Helvetica,sans-serif;">
		 * The three canonical forms are defined as
		 * <b>Evaluation<b> Computation of the probability (or likelihood) of the observed sequence 
		 * Ot given a Lambda (pi, A, B) model<br>
		 * <b>Training</b> Generation of the Lambda (pi, A, B)-model given a set of observations and 
		 * a sequence of states.<br>
		 * <b>Decoding</b> Extraction the most likely sequence of states {qt} given a set of 
		 * observations Ot and a Lambda (pi, A, B)-model.</span></pre></p>
		 * 
		 * @constructor Create a HMM algorithm with either a predefined Lambda model for evaluation 
		 * and prediction or a Lambda model to generate through training
		 * @throws IllegalArgumentException if the any of the class parameters is undefined
		 * @param lambda lambda model generated through training or used as input for the evaluation 
		 * and decoding phase
		 * @param form     Canonical form (evaluation or decoding) used in the prediction of sequence
		 * @param maxIters Maximum number of iterations used in the Baum-Welch algorithm
		 * @param f  Implicit conversion of a Double vector a parameterized type bounded to 
		 * Array[Int] (Discretization)
		 * @throws ImplicitNotFoundException if the implicit conversion from DblVector to T is 
		 * not defined prior the instantiation of the class
		 * @see org.scalaml.supervised.hmm.HMMLambda
		 * @see org.scalaml.supervised.hmm.HMMState
		 * @author Patrick Nicolas
		 * @since March 23, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model
		 */
@implicitNotFound("HMM Conversion from DblVector to type T undefined")
final protected class HMM[@specialized T <% Array[Int]](
		lambda: HMMLambda, 
		form: HMMForm, 
		maxIters: Int)
		(implicit f: DblVector => T)	extends PipeOperator[DblVector, HMMPredictor] {
	
	check(maxIters)
	
	private val logger = Logger.getLogger("HMM")
	private[this] val state = HMMState(lambda, maxIters)
	
		/**
		 * <p>Classifier for the Hidden Markov Model. The pipe operator evaluates the 
		 * HMM if form == EVALUATION or decodes a HMM if form == DECODING for a given
		 * set of observations, obs and a lambda model.</p>
		 * @throws MatchError if the observations sequence is not defined
		 * @return PartialFunction of a sequence of observations as input and a tuple 
		 * (likelihood, sequence of observation indices)
		 */
	override def |> : PartialFunction[DblVector, HMMPredictor] = {
		case obs: DblVector if( !obs.isEmpty) => {
			Try { 
				form match {
					case EVALUATION => evaluate(obs)
					case DECODING => decode(obs)
				} 
			} match {
				case Success(prediction) => prediction
				case Failure(e) => 
					DisplayUtils.error("HMM.|> ", logger, e)
					nullHMMPredictor
			}
		}
	}
	
		/**
		 * <p>Implements the 3rd canonical form of the HMM</p>
		 * @param obsIdx given sequence of observations
		 * @return HMMPredictor predictor as a tuple of (likelihood, sequence (array) of 
		 * observations indexes)
		 */
	def decode(obs: T): HMMPredictor = (ViterbiPath(lambda, obs).maxDelta, state.QStar())
	
		/**
		 * <p>Implements the 'Evaluation' canonical form of the HMM</p>
		 * @param obsIdx index of the observation O in a sequence
		 * @return HMMPredictor predictor as a tuple of (likelihood, sequence (array) of observations 
		 * indexes)
		 */
	def evaluate(obs: T): HMMPredictor = (-Alpha(lambda, obs).logProb, obs)
	
		/**
		 * <p>Retrieve the Lambda model associated to this HMM</p>
		 * @return lambda model
		 */
	final def getModel: HMMLambda = lambda
}



	/**
	 * <p>Companion object for the HMM that defines a HMMPredictor type and the constructors
	 * for the HMM and validate its input parameters</p>
	 * @author Patrick Nicolas
	 * @since March 11, 2014
	 */
object HMM {
		/**
		 * <p>Define the result of the prediction (decoding or evaluation) as a
		 * a tuple of (likelihood, sequence (array) of observations indexes).</p>
		 */
	type HMMPredictor = (Double, Array[Int])
	val nullHMMPredictor = (-1.0, Array.empty[Int])
	
		/**
		 * Default constructor for the Hidden Markov Model classifier (HMM)
		 * @param lambda lambda model generated through training or used as input for the evaluation 
		 * and decoding phase
		 * @param form     Canonical form (evaluation or decoding) used in the prediction of sequence
		 * @param maxIters Maximum number of iterations used in the Baum-Welch algorithm
		 * @param f  Implicit conversion of a Double vector a parameterized type bounded to 
		 * Array[Int] (Discretization)
		 */
	def apply[T <% Array[Int]](
			lambda: HMMLambda, 
			form: HMMForm, 
			maxIters: Int)
			(implicit f: DblVector => T): HMM[T] =	new HMM[T](lambda, form, maxIters)

		/**
		 * Constructor for the Hidden Markov Model classifier (HMM) with a predefined maximum 
		 * number of iterations
		 * @param lambda lambda model generated through training or used as input for the evaluation 
		 * and decoding phase
		 * @param form     Canonical form (evaluation or decoding) used in the prediction of sequence
		 * @param f  Implicit conversion of a Double vector a parameterized type bounded to
		 *  Array[Int] (Discretization)
		 */
	def apply[T <% Array[Int]](
			lambda: HMMLambda, 
			form: HMMForm)
			(implicit f: DblVector => T): HMM[T] =  new HMM[T](lambda, form, HMMState.DEFAULT_MAXITERS)
	
	
		/**
		 * Constructor for the Hidden Markov Model classifier (HMM) which model has
		 * to be initialized by the Baum-Welch algorithm. The objective is to generate
		 * the components A, B and pi of the lambda model
		 * @param config Configuration parameters for the HMM
		 * @param obsIndx Array of index of observations
		 * @param form     Canonical form (evaluation or decoding) used in the prediction of sequence
		 * @param maxIters Maximum number of iterations used in the Baum-Welch algorithm
		 * @param eps Convergence criteria used in the Baum-Welch algorithm (HMM training)
		 * @param f  Implicit conversion of a Double vector a parameterized type bounded to 
		 * Array[Int] (Discretization)
		 */
	def apply[T <% Array[Int]](
			config: HMMConfig, 
			obsIndx: Array[Int], 
			form: HMMForm,  
			maxIters: Int, 
			eps: Double)
			(implicit f: DblVector => T): Option[HMM[T]] = {
	  
		val baumWelchEM = new BaumWelchEM(config, obsIndx, maxIters, eps)
		baumWelchEM.maxLikelihood.map(_ => new HMM[T](baumWelchEM.lambda, form, maxIters))
	}
	
	val MAX_NUM_ITERS = 1024
	private def check(maxIters: Int): Unit = {
		require(maxIters > 1 && maxIters < MAX_NUM_ITERS, 
		    s"HMM.check  Maximum number of iterations to train a HMM $maxIters is out of bounds")
	}
}

// ----------------------------------------  EOF ------------------------------------------------------------
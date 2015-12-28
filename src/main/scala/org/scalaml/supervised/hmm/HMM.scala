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

import scala.util.Try
import scala.annotation.implicitNotFound

import org.apache.log4j.Logger

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.ITransform
import org.scalaml.util.LoggingUtils._
import org.scalaml.supervised.hmm.train.BaumWelchEM
import org.scalaml.supervised.hmm.eval.Alpha
import org.scalaml.supervised.hmm.decode.ViterbiPath
import HMM._

	/**
	 * case classes to specify the canonical form of the HMM
	 * @author Patrick Nicolas
	 * @since 0.98 March 9, 2014
	 * @see Scala for Machine Learning Chapter 7 ''Sequential Data Models'' / Hidden Markov Model
	 */
sealed trait HMMForm

	/**
	 * Class that defines the Evaluation (CF-1) Canonical form of the HMM
	 */
case class EVALUATION() extends HMMForm 

	/**
	 * Class that defines the Decoding (CF-2) Canonical form of the HMM
	 */
case class DECODING() extends HMMForm


		/**
		 * Implementation of the Hidden Markov Model (HMM). The HMM classifier defines the
		 * three canonical forms (Decoding, training and evaluation).
		 * 
		 * The three canonical forms are defined as
		 * - '''Evaluation''' Computation of the probability (or likelihood) of the observed sequence 
		 * Ot given a Lambda (pi, A, B) model
		 * - '''Training''' Generation of the Lambda (pi, A, B)-model given a set of observations and 
		 * a sequence of states.
		 * - '''Decoding''' Extraction the most likely sequence of states {qt} given a set of 
		 * observations Ot and a Lambda (pi, A, B)-model
		 * 
		 * The implementation follows the standard design of supervised learning algorithm:
		 * - The classifier implements the '''ITransform''' implicit monadic data transformation
		 * - The constructor triggers the training of the classifier, making the model immutable
		 * - The classifier implements the '''Monitor''' interface to collect profile information for 
		 * debugging purpose
		 * 
		 * @tparam T type of elements of the input data
		 * @constructor Create a HMM algorithm with either a predefined Lambda model for evaluation 
		 * and prediction or a Lambda model to generate through training
		 * @throws IllegalArgumentException if the any of the class parameters is undefined
		 * @param config Configuration for the HMM
		 * @param xt Time series of observations for which the hidden states have to be extracted
		 * @param form Canonical form (evaluation or decoding) used in the prediction of sequence
		 * @param quantize  Implicit conversion of a Double vector a parameterized type bounded to 
		 * Array[Int] (Discretization)
		 * @param f Implicit conversion from a T to a Double
		 * @throws ImplicitNotFoundException if the implicit conversion from DblArray to T is 
		 * not defined prior the instantiation of the class

		 * @author Patrick Nicolas
		 * @since 0.98.2 March 23, 2014
		 * @version 0.99.1
		 * @see org.scalaml.supervised.hmm.HMMModel
		 * @see org.scalaml.supervised.hmm.HMMConfig
		 * @see Scala for Machine Learning Chapter 7 ''Sequential data models'' / Hidden Markov Model
		 */
@throws(classOf[IllegalArgumentException])
@implicitNotFound(msg = "HMM Conversion  $T => Double undefined")
@implicitNotFound(msg = "HMM Quantization Array[$T] => Int undefined")
final protected class HMM[@specialized(Double) T <: AnyVal](
		config: HMMConfig,
		xt: XVSeries[T],
		form: HMMForm)
		(implicit quantize: Array[T] => Int, f: T => Double) 
	extends ITransform[Array[T]](xt) with Monitor[Double] {
	
	type V = HMMPrediction

	protected val obsSeq: Vector[Int] = xt.map(quantize(_))

	
	protected val logger = Logger.getLogger("HMM")
	
		/**
		 * Model for the hidden Markov model 
		 */
	private[this] val model: Option[HMMModel] = train

	
		/**
		 * Test if this model has been successfully generated and the state transition,
		 * emission and initial probabilities are correctly normalized.
		 * @return true if the model is valid, false otherwise.
		 */
	final val isModel: Boolean = model.isDefined && model.get.validate(config.eps)
	
	private def train: Option[HMMModel] = Try {
		val baumWelchEM = BaumWelchEM(config, obsSeq)
		baumWelchEM.lambda
	}._toOption("HMM.BaumWelch", logger)

		/**
		 * Classifier for the Hidden Markov Model. The pipe operator evaluates the 
		 * HMM if form == EVALUATION or decodes a HMM if form == DECODING for a given
		 * set of observations, obs and a lambda model.
		 * @throws MatchError if the observations sequence is not defined
		 * @return PartialFunction of a sequence of observations as input and a tuple 
		 * (likelihood, sequence of observation indices)
		 */
	override def |> : PartialFunction[Array[T], Try[V]] = {
		case ySeq: Array[T] if isModel && ySeq.length > 1 => form match {
			case _ : EVALUATION => evaluation(model.get, Vector[Int](quantize(ySeq)))
			case _ : DECODING => decoding(model.get, Vector[Int](quantize(ySeq)))
		}
	}

		/**
		 * Retrieve the Lambda model associated to this HMM
		 * @return lambda model
		 */
	final def getModel: Option[HMMModel] = model
	
	override def toString: String = model.map(_.toString).getOrElse("HMM undefined")
}



	/**
	 * Companion object for the HMM that defines a HMMPredictor type and the constructors
	 * for the HMM and validate its input parameters
	 * @author Patrick Nicolas
	 * @since March 11, 2014
	 */
object HMM {
	private val logger = Logger.getLogger("HMM")

		/**
		 * Constructor for the Hidden Markov Model classifier (HMM) which model has
		 * to be initialized by the Baum-Welch algorithm. The objective is to generate
		 * the components A, B and pi of the lambda model
		 * @tparam T Type of observed states
		 * @param config Configuration parameters for the HMM
		 * @param xt time series of observed values.
		 * @param form  Canonical form (evaluation or decoding) used in the prediction of sequence
		 * @param f  Implicit conversion of a Double vector a parameterized type bounded to 
		 * Array[Int] (Discretization)
		 */
	@implicitNotFound(msg = "HMM Quantization is not defined for type $T")
	def apply[T <: AnyVal](
			config: HMMConfig, 
			xt: XVSeries[T], 
			form: HMMForm)
			(implicit quantize: Array[T] => Int, f: T => Double): HMM[T] = 
		new HMM[T](config, xt, form)
	  
	 
		/**
		 * Implements the 3rd canonical form 'decoding' of the HMM
		 * @tparam T Type of observed states
		 * @param model  Lambda model composed of state transition, emission and initial probability
		 * @param xt A given sequence of observations
		 * @return HMMPredictor predictor as a tuple of (likelihood, sequence (array) of 
		 * observations indexes)
		 */
	@implicitNotFound(msg = "HMM Quantization is not defined for type $T")
	def decode[T <: AnyVal](
			model: HMMModel, 
			xt: XVSeries[T])
			(implicit quantize: Array[T] => Int, f: T => Double): Option[HMMPrediction] =
		decoding(model, xt.map(quantize(_)))._toOption("HMM.decode", logger)

	protected def decoding(model: HMMModel, obsSeq: Vector[Int]): Try[HMMPrediction] = Try { 
		ViterbiPath(model, obsSeq).path
	}
	
		/**
		 * Implements the 'Evaluation' canonical form of the HMM
		 * @tparam T Type of observed states
		 * @param model  Lambda model composed of state transition, emission and initial probability
		 * @param quantize quantization function
		 * @return HMMPredictor predictor as a tuple of (likelihood, sequence (array) of observations 
		 * indexes)
		 */
	@implicitNotFound(msg = "HMM Quantization is not defined for type $T")
	def evaluate[T <: AnyVal](
			model: HMMModel, 
			xt: XVSeries[T])
			(implicit quantize: Array[T] => Int, f: T => Double): Option[HMMPrediction] =  
		evaluation(model, xt.map(quantize(_)))._toOption("HMM.evaluate", logger)

	
	protected def evaluation(model: HMMModel, obsSeq: Vector[Int]): Try[HMMPrediction] =  Try {
		HMMPrediction(-Alpha(model, obsSeq).logProb, obsSeq.toArray) 
	}
		
		
		
	val MAX_NUM_ITERS = 1024
	private def check(maxIters: Int): Unit = {
		require(maxIters > 1 && maxIters < MAX_NUM_ITERS, 
				s"HMM.check  Maximum number of iterations to train a HMM $maxIters is out of bounds")
	}
}

// ----------------------------------------  EOF ------------------------------------------------------------
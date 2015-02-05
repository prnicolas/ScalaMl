/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used 
 * to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98.1
 */
package org.scalaml.app.chap7

import org.scalaml.util.DisplayUtils
import org.scalaml.supervised.hmm._
import org.scalaml.core.Types.ScalaMl.{DblVector, DblMatrix}
import org.scalaml.core.Matrix
import org.scalaml.app.Eval

		/**
		 * <p><b>Purpose:</b> Singleton for the evaluation of Hidden Markov Models presented in 
		 * chapter 7<br>
		 * Symbols: Stock up/down (1, 0)<br>
		 * States: 6 normalized ratio of % bullish investors / % bearish investors discretized in 
		 * 6 different levels<br>
		 * </p>
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning Chapter 7 Sequential data models/Hidden Markov model
		 */
object HMMEval extends Eval  {
	import scala.io.Source	
	import scala.util.{Try, Success, Failure}
	import scala.language.implicitConversions
	import org.apache.log4j.Logger
	import HMM._
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "HMMEval"
	
	private val STATES_PATH = "resources/data/chap7/statesprob.csv"
	private val OBS_PATH = "resources/data/chap7/obsprob.csv"
	private val CSV_DELIM= ","
	private val NUM_SYMBOLS = 6
	private val NUM_STATES = 5
	private val EPS = 1e-4
	private val MAX_ITERS = 150

	implicit def discretize(x: DblVector): Array[Int] = x.map(_.toInt) 

		/** <p>Execution of the scalatest for <b>HMM</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = args(0) match {
		case "evaluation" => runCF1
		case "training" => runCF2
		case "decoding" => runCF3
		case _ => DisplayUtils.error(s"$name.run: Incorrect argument $args", logger)
	}

		// Method that evaluates the 'Evaluation' canonical form of the Hidden
		// Markov model: compute the likelihood of an observed sequence given a
		// defined Lambda model
	private def runCF1: Int = {
		DisplayUtils.show(s"$header Hidden Markov Model - Evaluation", logger)

		// Step 1. Defined the Lambda model
		// State-transition probabilities matrix for HMM
		val A0 = Array[Array[Double]](
			Array[Double](0.21, 0.13, 0.25, 0.06, 0.11, 0.24),
			Array[Double](0.31, 0.17, 0.18, 0.04, 0.19, 0.11),
			Array[Double](0.32, 0.15, 0.15, 0.01, 0.21, 0.16),
			Array[Double](0.25, 0.12, 0.11, 0.01, 0.27, 0.24),
			Array[Double](0.22, 0.10, 0.09, 0.03, 0.31, 0.25),
			Array[Double](0.15, 0.08, 0.05, 0.07, 0.43, 0.22)
		)
  	  		
		// Emission/observations probabilities matrix
		val B0 =  Array[Array[Double]](
			Array[Double](0.61, 0.39),
			Array[Double](0.54, 0.46),
			Array[Double](0.45, 0.55),
			Array[Double](0.46, 0.54),
			Array[Double](0.33, 0.67),
			Array[Double](0.42, 0.58)
		)

		val PI0 = Array[Double](0.26, 0.04, 0.11, 0.26, 0.19, 0.14)
		val NUM_OBS: Int = 12
		val lambda = HMMLambda(Matrix[Double](A0), Matrix[Double](B0), PI0, NUM_OBS)
		
		// Step 2: Defined the observed sequence	
		val observedSeq = Array[Double](
			0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 0.0
		)
		
		// Step 3: Compute the probability of observing observedQ
		val hmm = HMM[Array[Int]](lambda, EVALUATION())
		Try( hmm |> observedSeq )		  
				.map(p => {
					val indices = p._2.mkString(" ")
				  DisplayUtils.show(s"$name Likelihood: ${p._1.toString}\nindices: $indices", logger)
				})
				.getOrElse(DisplayUtils.error(s"$name (Evaluation) failed", logger))
	}
	

		// Method that illustrates the 'Training' canonical form of the Hidden
		// Markov model: extract or learn a lambda model given a set of observations.
	private def runCF2: Int =  {
		DisplayUtils.show(s"$header Hidden Markov Model - Training", logger)
		
		// Step 1: Specifies the sequence of observations
		val observations = Array[Double](
			0.01, 0.72, 0.78, 0.56, 0.61, 0.56, 0.45, 0.42, 0.46, 0.38, 0.35, 0.31, 0.32, 0.34, 
			0.29, 0.23, 0.21, 0.24, 0.18, 0.15, 0.11, 0.08, 0.10, 0.03, 0.00, 0.06, 0.09, 0.13, 
			0.11, 0.17, 0.22, 0.18, 0.25, 0.30, 0.29, 0.36, 0.37, 0.39, 0.38, 0.42, 0.46, 0.43, 
			0.47, 0.50, 0.56, 0.52, 0.53, 0.55, 0.57, 0.60, 0.62, 0.65, 0.68, 0.65, 0.69, 0.72, 
			0.76, 0.82, 0.87, 0.83, 0.90, 0.88, 0.93, 0.92, 0.97, 0.99, 0.95, 0.91
		)
		val stateSeq =  Array[Int](
			4, 1, 3, 1, 0, 0, 1, 2, 0, 1, 5, 5, 3, 4, 1, 1, 5, 3, 1, 3, 2, 1, 4, 5, 2, 
			1, 3, 4, 5, 0, 1, 1, 4, 2, 3, 4
		)
		
		val config = new HMMConfig(stateSeq.size, NUM_STATES, NUM_SYMBOLS)
		
		// Step 2 Extract the HMM model hmm.model
		HMM[Array[Int]](config, stateSeq, TRAINING(), MAX_ITERS, EPS)
				.map(hmm => DisplayUtils.show(s"$name (Training):\n${hmm.getModel.toString}", logger) )
				.getOrElse(DisplayUtils.error(s"$name (Training) failed", logger))
	}

	
		// Method that illustrates the 'Decoding' canonical form of the Hidden
		// Markov model: identify the most likely sequence of states given a 
		// lambda model and a set of observations.
	private def runCF3: Int = {
		DisplayUtils.show(s"$header Hidden Markov Model - Decoding", logger)
		
		// Step 1: Define the lambda model
		// State-transition probabilities matrix for HMM
		val A0 = Array[Array[Double]](
			Array[Double](0.2, 0.7, 0.1),
			Array[Double](0.0, 0.1, 0.5),
			Array[Double](0.8, 0.2, 0.4)
		)
  	  		
		// Emission/observations probabilities matrix
		val B0 =  Array[Array[Double]](
			Array[Double](0.1, 0.9),
			Array[Double](0.3, 0.7),
			Array[Double](0.4, 0.6)
		)

		val PI0 = Array[Double](0.3, 0.2, 0.5)
		
		// Step 2: Specifies the sequence of observations 
		val observedSeq = Array[Double](
			1.0, 2.0, 8.9, 3.3, 1.1, 0.4, 0.5, 0.3, 0.8, 0.7, 1.8, 3.9, 6.0, 9.2, 11.7
		)
		
		// Step 3: Decode the sequence of states using Viterbi algorithm
		val lambda = HMMLambda(Matrix[Double](A0), Matrix[Double](B0), PI0, observedSeq.size)
		val hmm = HMM[Array[Int]](lambda, DECODING())
		
		Try( hmm |> observedSeq )
				.map(p => DisplayUtils.show(s"$name State sequence\n${p._2.mkString(", ")}", logger))
				.getOrElse(-1)
	}
}


object MyApp extends App {
  HMMEval.run(Array[String]("decoding"))
}
// --------------------------------  EOF -------------------------------
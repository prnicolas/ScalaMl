/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.app.chap7

import org.scalaml.util.DisplayUtils
import org.scalaml.supervised.hmm.{HMM, HMMForm, HMMLambda, HMMConfig}
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
	import HMM._, HMMForm._
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "HMMEval"
	
	private val STATES_PATH = "resources/data/chap7/statesprob.csv"
	private val OBS_PATH = "resources/data/chap7/obsprob.csv"
	private val CSV_DELIM= ","
	private val NUM_SYMBOLS = 6
	private val NUM_STATES = 5
	private val EPS = 1e-3
	private val MAX_ITERS = 250

	implicit def discretize(x: DblVector): Array[Int] = x.map(_.toInt) 
   		
		/** <p>Execution of the scalatest for <b>HMM</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = args(0) match {
		case "evaluation" => runCF1
		case "training" => runCF2
		case _ => DisplayUtils.error(s"$name.run: Incorrect argument $args", logger)
	}
   

	private def runCF2: Int =  {
		DisplayUtils.show(s"$header Hidden Markov Model - Training", logger)
  	  
		val observations = Array[Double](
			0.0, 0.72, 0.78, 0.56, 0.61, 0.56, 0.45, 0.42, 0.46, 0.38, 
			0.35, 0.31, 0.32, 0.34, 0.29, 0.23, 0.21, 0.24, 0.18, 0.15, 
			0.11, 0.08, 0.10, 0.03, 0.0, 0.06, 0.09, 0.13, 0.11, 0.17, 
			0.22, 0.18, 0.25, 0.30, 0.29, 0.36, 0.37, 0.39, 0.38, 0.42, 
			0.46, 0.43, 0.47, 0.50, 0.56, 0.52, 0.53, 0.55, 0.57, 0.60, 
			0.62, 0.65, 0.68, 0.65, 0.69, 0.72, 0.76, 0.82, 0.87, 0.83, 
			0.90, 0.88, 0.93, 0.92, 0.97, 0.99, 0.95, 0.91
		)
  	   
		val min = observations.min
		val delta = observations.max - min

		val obsSeq =  Array[Int](
			4, 1, 3, 1, 0, 0, 1, 2, 0, 1, 5, 5, 
			3, 4, 1, 1, 5, 3, 1, 3, 2, 1, 4, 5, 2, 
			1, 3, 4, 5, 0, 1, 1, 4, 2, 3, 4
		)
		val config = new HMMConfig(obsSeq.size, NUM_STATES, NUM_SYMBOLS)
		
		val hmm = HMM[Array[Int]](config, obsSeq, EVALUATION, MAX_ITERS, EPS) 
		hmm match {
			case Some( hmm) => DisplayUtils.show(s"$name (Training):\n${hmm.getModel.toString}", logger)
			case None => DisplayUtils.error("$name (Training) lambda model could not be created", logger)
		}
	}
   
   
	private def runCF1: Int = {
		DisplayUtils.show(s"$header Hidden Markov Model - Evaluation", logger)
  	  		
		// State-transition probabilities matrix for HMM
		val A0 = Array[Array[Double]](
			Array[Double](0.21, 0.23, 0.45, 0.56, 0.31, 0.30),
			Array[Double](0.31, 0.17, 0.28, 0.44, 0.39, 0.11),
			Array[Double](0.08, 0.13, 0.33, 0.15, 0.48, 0.39),
			Array[Double](0.16, 0.10, 0.26, 0.56, 0.42, 0.19),
			Array[Double](0.28, 0.18, 0.35, 0.31, 0.38, 0.20),
			Array[Double](0.31, 0.22, 0.47, 0.51, 0.35, 0.24)
		)
  	  		
		// Emission/observations probabilities matrix
		val B0 =  Array[Array[Double]](
			Array[Double](0.41, 0.17),
			Array[Double](0.34, 0.28),
			Array[Double](0.23, 0.42),
			Array[Double](0.16, 0.24),
			Array[Double](0.33, 0.46),
			Array[Double](0.42, 0.37)
		)

		val PI0 = Array[Double](0.26, 0.34, 0.11, 0.56, 0.39, 0.47)
		val NUM_OBS: Int = 12
		val lambda = HMMLambda(Matrix[Double](A0), Matrix[Double](B0), PI0, NUM_OBS)

		val hmm = HMM[Array[Int]](lambda, EVALUATION)
	
		val observedSeq = Array[Double](
			0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 0.0
		)
		
		Try( hmm |> observedSeq ) match {
			case Success(predictor) => {
				val indices = predictor._2.mkString(" ")
				DisplayUtils.show(s"$name Likelihood: ${predictor._1.toString}\nindices: $indices", logger)
			}
			case Failure(e) => failureHandler(e)
		}
	}
}


// --------------------------------  EOF -------------------------------
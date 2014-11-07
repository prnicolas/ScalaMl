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
package org.scalaml.app.chap7

import org.scalaml.util.Matrix
import org.scalaml.supervised.hmm.{HMM, HMMForm, HMMLambda}
import org.apache.log4j.Logger
import org.scalaml.util.Display
import scala.io.Source
import org.scalaml.core.types.ScalaMl._
import scala.util.{Try, Success, Failure}
import scala.language.implicitConversions
import org.scalaml.supervised.hmm.HMMConfig
import org.scalaml.app.Eval

		/**
		 * <p>Singleton for the evaluation of Hidden Markov Models presented in chapter 7<br>
		 * Symbols: Stock up/down (1, 0)<br>
		 * States: 6 normalized ratio of % bullish investors / % bearish investors discretized in 6 different levels<br>
		 * </p>
		 * @author Patrick Nicolas
		 * @since March 28, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models $Hidden Markov model
		 */
object HMMEval extends Eval  {
   import HMM._, HMMForm._
   val name: String = "HMMEval"
  	 
   final val STATES_PATH = "resources/data/chap7/statesprob.csv"
   final val OBS_PATH = "resources/data/chap7/obsprob.csv"
   final val CSV_DELIM= ","
   final val NUM_SYMBOLS = 6
   final val NUM_STATES = 5
   final val EPS = 1e-3
   final val MAX_ITERS = 250
   
   private val logger = Logger.getLogger(name)
   
   def run(args: Array[String]): Int = args(0) match {
  	  case "evaluation" => runCF1
  	  case "training" => runCF2
  	  case _ => Display.error("HMMEval.run: Incorrect argument $args", logger)
   }
   
   def runCF2: Int =  {
  	  Display.show("Hidden Markov Models: Training form", logger)
  	  
  	  val observations = Array[Double](
  	       0.0, 0.72, 0.78, 0.56, 0.61, 0.56, 0.45, 0.42, 0.46, 0.38, 
  	       0.35, 0.31, 0.32, 0.34, 0.29, 0.23, 0.21, 0.24, 0.18, 0.15, 
  	       0.11, 0.08, 0.10, 0.03, 0.0, 0.06, 0.09, 0.13, 0.11, 0.17, 
  	       0.22, 0.18, 0.25, 0.30, 0.29, 0.36, 0.37, 0.39, 0.38, 0.42, 
  	       0.46, 0.43, 0.47, 0.50, 0.56, 0.52, 0.53, 0.55, 0.57, 0.60, 
  	       0.62, 0.65, 0.68, 0.65, 0.69, 0.72, 0.76, 0.82, 0.87, 0.83, 
  	       0.90, 0.88, 0.93, 0.92, 0.97, 0.99, 0.95, 0.91)
  	   
  	  val min = observations.min
  	  val delta = observations.max - min
  	//  val obsSeq = observations.map( x => (x - min)/delta).map(x => (x*NUM_SYMBOLS).floor.toInt)
  	val obsSeq =  Array[Int](4, 1, 3, 1, 0, 0, 1, 2, 0, 1, 5, 5, 3, 4, 1, 1, 5, 3, 1, 3, 2, 1, 4, 5, 2, 1, 3, 4, 5, 0, 1, 1, 4, 2, 3, 4)
  	  val config = new HMMConfig(obsSeq.size, NUM_STATES, NUM_SYMBOLS)
  	  
  	  implicit def discretize(x: DblVector): Array[Int] = x.map(_.toInt) 
  	  HMM[Array[Int]](config, obsSeq, EVALUATION, MAX_ITERS, EPS) match {
  	  	 case Some( hmm) => Display.show(s"HMMEval,runCF2:\n${hmm.getModel.toString}", logger)
  	  	 case None => Display.error("HMMEval.runCF2: lambda model could not be created", logger)
  	  }
  }
   
   
   def runCF1: Int = {
  	  Display.show("Hidden Markov Models: Evaluation form", logger)
  	  		// Transition matrix for HMM
  	  val A0 = Array[Array[Double]](
  	     Array[Double](0.21, 0.23, 0.45, 0.56, 0.31, 0.30),
  	     Array[Double](0.31, 0.17, 0.28, 0.44, 0.39, 0.11),
  	     Array[Double](0.08, 0.13, 0.33, 0.15, 0.48, 0.39),
  	     Array[Double](0.16, 0.10, 0.26, 0.56, 0.42, 0.19),
  	     Array[Double](0.28, 0.18, 0.35, 0.31, 0.38, 0.20),
  	     Array[Double](0.31, 0.22, 0.47, 0.51, 0.35, 0.24)
  	  )
  	  		// Emission/observations matrix
  	  
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
  	  	 

	
	//  implicit def discretize(x: DblVector): Array[Int] = x.map(_ * NUM_STATES).map( _.floor.toInt)
  	  
  	  implicit def discretize(x: DblVector): Array[Int] = x.map(_.toInt) 
	  val hmm = HMM[Array[Int]](lambda, EVALUATION)
	
      val observedSeq = Array[Double](0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 0.0)
	  Try( hmm |> observedSeq )
	  match {
	     case Success(predictor) => {
	       val indices = predictor._2.foldLeft(new StringBuilder)((b, p) => b.append(s"$p, "))
	       Display.show(s"HMMEval.run (CF1 Evaluation)  Likelihood: ${predictor._1.toString} , indices: $indices", logger)
	     }
	     case Failure(e) => Display.error("HMMEval.run", logger, e)
	  }
   }
}


object HMMEvalApp extends App {
  HMMEval.runCF2
}

// --------------------------------  EOF -------------------------------
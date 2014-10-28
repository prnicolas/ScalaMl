/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.94
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

		/**
		 * Singleton for the evaluation of Hidden Markov Models presented in chapter 7
		 * 
		 * @author Patrick Nicolas
		 * @since March 28, 2014
		 * @note Scala for Machine Learning
		 */
object HMMEval {
  import HMM._, HMMForm._

   final val STATES_PATH = "resources/data/chap7/statesprob.csv"
   final val OBS_PATH = "resources/data/chap7/obsprob.csv"
   final val CSV_DELIM=","
   private val logger = Logger.getLogger("HMMEval")
   
  def run: Int = {
  		
  	Display.show("Evaluation of Hidden Markov Models", logger)

    val srcStates =  Source.fromFile(STATES_PATH)
	val statesProb: Seq[DblVector] = srcStates.getLines.map( _.split(CSV_DELIM).map(_.toDouble)).toSeq
		   
	val srcObs =  Source.fromFile(OBS_PATH)
	val obsProb: Seq[DblVector] = srcObs.getLines.map( _.split(CSV_DELIM).map(_.toDouble)).toSeq
	
	val lambda = HMMLambda(statesProb, obsProb)
	
	
	implicit def discretize(x: DblVector): Array[Int] = x.map(_ * 20).map( _.floor.toInt)
	val hmm = HMM[Array[Int]](lambda, EVALUATION)
	
	val percentBullish = 0.35
	val percentNeutral = 0.2
	val percentBearish = 0.45
	
	Try( hmm |> Array[Double](percentBullish, percentNeutral, percentBearish) )
	match {
	  case Success(predictor) => Display.show(s"HMMEval.run Likelihood: ${predictor.toString}", logger)
	  case Failure(e) => Display.error("HMMEval.run", logger, e)
	}
  }
}



// --------------------------------  EOF -------------------------------
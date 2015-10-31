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
 * Version 0.99
 */
package org.scalaml.app.chap7

import scala.io.Source	
import scala.util.{Try, Success, Failure}
import scala.language.implicitConversions
import scala.reflect.ClassTag
import org.apache.log4j.Logger


import org.scalaml.util.DisplayUtils
import org.scalaml.supervised.hmm.{HMM, HMMModel, HMMConfig, EVALUATION}
import org.scalaml.core.Types.ScalaMl.{DblArray, DblMatrix, XVSeries}
import org.scalaml.util.MathUtils._
import org.scalaml.app.Eval
import DisplayUtils._, HMM._

		/**
		 * '''Purpose:''' Singleton for the evaluation of Hidden Markov Models presented in 
		 * chapter 7
		 * 
		 * Symbols: Stock up/down (1, 0)
		 * 
		 * States: 6 normalized ratio of % bullish investors / % bearish investors discretized in 
		 * 6 different levels
		 * 
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 7 ''Sequential data models'' / Hidden Markov model
		 */
object HMMTrainingEval extends Eval  {
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "HMMTrainingEval"
	
	private val STATES_PATH = "resources/data/chap7/statesprob.csv"
	private val OBS_PATH = "resources/data/chap7/obsprob.csv"
	private val CSV_DELIM= ","
	private val NUM_SYMBOLS = 6
	private val NUM_STATES = 5
	private val EPS = 1e-4
	private val MAX_ITERS = 150

	

		/** Execution of the scalatest for '''HMM''' class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate
		 * 	 
		 * Exceptions thrown during the execution of the tests are caught by the wrapper or handler
		 * test method in Eval trait defined as follows:
		 * {{{
		 *    def test(args: Array[String]) =
		 *      Try(run(args)) match {
		 *        case Success(n) => ...
		 *        case Failure(e) => ...
		 * }}}
		 * The tests can be executed through ''sbt run'' or individually by calling 
		 * ''TestName.test(args)'' (i.e. DKalmanEval.test(Array[String]("IBM") )
		 * @param args array of arguments used in the test
		 */
	override protected def run(args: Array[String]): Int = {

		// Step 1: Specifies the sequence of observations
		val observations = Vector[Double](
			0.01, 0.72, 0.78, 0.56, 0.61, 0.56, 0.45, 0.42, 0.46, 0.38, 0.35, 0.31, 0.32, 0.34, 
			0.29, 0.23, 0.21, 0.24, 0.18, 0.15, 0.11, 0.08, 0.10, 0.03, 0.00, 0.06, 0.09, 0.13, 
			0.11, 0.17, 0.22, 0.18, 0.25, 0.30, 0.29, 0.36, 0.37, 0.39, 0.38, 0.42, 0.46, 0.43, 
			0.47, 0.50, 0.56, 0.52, 0.53, 0.55, 0.57, 0.60, 0.62, 0.65, 0.68, 0.65, 0.69, 0.72, 
			0.76, 0.82, 0.87, 0.83, 0.90, 0.88, 0.93, 0.92, 0.97, 0.99, 0.95, 0.91
		)

		
		implicit val quantize = (x: DblArray) => (x.head* (NUM_STATES+1)).floor.toInt
		
		val xt: XVSeries[Double] = observations.map(Array[Double](_))
		val config = HMMConfig(xt.size, NUM_STATES, NUM_SYMBOLS, MAX_ITERS, EPS)
		
		// Step 2 Extract the HMM model hmm.model
		val hmm = HMM[Double](config, xt, EVALUATION())
		
		if( hmm.isModel) 
			show(s"Lambda model for HMM generated through training is valid:\n${hmm.toString}")		
		else
			error("Training of hidden Markov model failed")
	}
}


// --------------------------------  EOF -------------------------------
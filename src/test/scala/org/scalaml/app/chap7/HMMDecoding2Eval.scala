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

import scala.util.{Try, Success, Failure}
import scala.language.implicitConversions
import scala.reflect.ClassTag
import org.apache.log4j.Logger


import org.scalaml.util.DisplayUtils
import org.scalaml.supervised.hmm.{HMM, HMMModel, HMMConfig, EVALUATION}
import org.scalaml.core.Types.ScalaMl.{DblArray, DblMatrix, XVSeries}
import org.scalaml.util.MathUtils._
import org.scalaml.stats.XTSeries._
import org.scalaml.app.Eval
import DisplayUtils._, HMM._

		/**
		 * '''Purpose:''' Singleton for the evaluation of Hidden Markov Models presented in 
		 * chapter 7 ''Sequential Data Models''
		 * 
		 * - Symbols: Characters {'A", 'B', 'C', 'D' }
		 * - Number of states 2
		 * - Number of symbols 4
		 * - Number of observations 13
		 * 
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 7 ''Sequential Data Models'' /Hidden Markov model
		 * @see org.scalaml.app.chap7.HMMDecoding2Eval
		 */
object HMMDecodingEval2 extends Eval  {
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "HMMDecodingEval"

			// Quantization function
	implicit def quantize(xt: DblArray): Int = xt.head.toInt


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
		import HMMModel._
		
		show(s"$header Hidden Markov Model - Decoding")
		
		val symbols = Map[Char, Int]('A' -> 0, 'B' -> 1, 'C' -> 2, 'D' ->3)

		// Step 1: Define the lambda model
		// State-transition probabilities matrix for HMM
		val A0 = Array[Array[Double]](
			Array[Double](0.4, 0.6),
			Array[Double](0.5, 0.5)
		)
  	  		
		// Emission/observations probabilities matrix for
		//[A, B, C, D]
		val B0 =  Array[Array[Double]](
				// C, A, D, D
			Array[Double](0.0, 0.0, 0.25, 0.75),
				// A, B, B, B
			Array[Double](0.25, 0.75, 0.0, 0.0)
		)

		val PI0 = Array[Double](0.6, 0.4)
		
		if( !validate(A0, B0, PI0))
			throw new IllegalStateException("HMMEvaluationEval incorrect lambda model")
		show("Lambda model validated")

		// Step 2: Specifies and normalized the sequence of observations 
		val observed = Vector[Char]('D', 'C', 'D', 'D', 'C', 'C', 'A', 'A', 'B', 'B', 'B', 'D', 'A')
		val xt = observed.flatMap( symbols.get(_)).map(Array[Double](_)).toVector
		show(s"Input ${xt.map(_.mkString).mkString(",")}")
		
		// Step 3: Decode the sequence of states using Viterbi algorithm
		val lambda = HMMModel(DMatrix(A0), DMatrix(B0), PI0, xt.length)
		decode(lambda, xt).map( _.toString)
			.map( show(_))
				.getOrElse(error("HMM decoding failed"))
	}
}


// --------------------------------  EOF -------------------------------
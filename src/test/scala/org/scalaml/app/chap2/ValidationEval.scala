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
package org.scalaml.app.chap2

import scala.util.{Try, Random}
import org.apache.log4j.Logger
import org.scalaml.app.Eval
import org.scalaml.core.Types.ScalaMl.{DblVector,DblArray}
import org.scalaml.util.{DisplayUtils,  LoggingUtils}
import org.scalaml.stats.{Stats, MinMax}
import LoggingUtils._
import org.scalaml.validation.BinFValidation
import org.scalaml.util.Assertable

		/**
		 * '''Purpose''' Singleton to evaluate basic statistics
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 2 "Hello World!"
		 */
object ValidationEval extends Eval with Assertable {

		/**
		 * Name of the evaluation 
		 */
	val name: String = "StatsEval"
	  
	protected val assertMsg: String = "Binomial validation"
	
	final val source = Vector[Int](1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0) 
	final val expected = Vector[Int](1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0)
	final val predict = (x: DblArray) => x(0).floor.toInt
	
	final val PRECISION = 0.78
	final val RECALL = 1.0
	final val F1 = 0.875
			/**
		 * Execution of the scalatest for '''Stats'''
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
		show(s"$header Basic validation")
	
			// Display the profile of different Fn score
		displayFnScore
		
			// Create a stats object and compute the normalized and zScored value
		val predicted = source.map(_.toDouble).map( Array[Double](_))
		val validator = new BinFValidation(expected, predicted)(predict)
		
		assertDouble(validator.precision, PRECISION, 1e-2)
		show(s"precision = ${validator.precision}")
		
		assertDouble(validator.recall, RECALL, 1e-2)
		show(s"recall = ${validator.recall}")
		
		assertDouble(validator.f1, F1, 1e-2)
		show(s"F1 = ${validator.f1}")
	}
	
	
	private def displayFnScore: Unit = {
	  import org.scalaml.plots._
       val info = Legend(
				"ValidationEval", 
				"Validation: F1, F2 and F3 score with recall = 0.5",
				"Precision",
				"F values"
			)
			
			val R = 0.5
			val f1Precision = (p: Double) => 2.0*p*R/(p + R)
			val f2Precision = (p: Double) => 5*p*R/(4*p + R)
			val f3Precision = (p: Double) => 10*p*R/(9*p + R)
		
			
			val entries = List[(DblVector, String)]( 
       (Vector.tabulate(98)(n => f1Precision(0.01*(n+1))), "F1 (precision)"),
       (Vector.tabulate(98)(n => f2Precision(0.01*(n+1))), "F2 (precision)"),
       (Vector.tabulate(98)(n => f3Precision(0.01*(n+1))), "F3 (precision)")
      )
     LinePlot.display(entries, info, new LightPlotTheme)
	}
}


// ----------------------------------- EOF -------------------------------------
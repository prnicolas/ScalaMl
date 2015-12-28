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
package org.scalaml.app.chap3

import scala.util.Try
import org.apache.log4j.Logger

import org.scalaml.core.Types.ScalaMl.{DblVector, DblArray}
import org.scalaml.stats.XTSeries
import org.scalaml.util.LoggingUtils
import org.scalaml.filtering.movaverage._
import org.scalaml.app.Eval
import LoggingUtils._, XTSeries._

import org.scalaml.util.Assertable

		/**
		 * '''Purpose:'''Singleton used to test the moving average algorithms
		 * @author Patrick Nicolas
		 * @since 0.98  (March 7, 2014)
		 * @version 0.98
		 * @see Scala for Machine Learning Chapter 3 "Data Pre-processing" / Moving averages
		 * @see org.scalaml.filtering.MovingAverage
		 */
object MovingAverageEval2 extends FilteringEval with Assertable {
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "MovingAveragesEval2"
	protected val assertMsg = "Moving average tests"
	
	final val PERIOD = 3
	final val WEIGHTS = Array[Double](0.2, 0.6, 0.2)

	final val input = Vector[Double](
		1.0, 2.0, 1.5, 2.0, 2.8, 3.2, 3.9, 4.6, 5.2, 4.0, 3.7, 3.1, 2.5, 2.2, 1.6, 1.2, 0.4
	)

	final val SIMPLE_MOVAVG_LABEL = Vector[Double](
		0.0,0.0,1.50,1.83, 2.10,2.66, 3.30,3.90,4.57,4.60,4.30,3.6,3.10,2.60,2.11,1.67,1.07
	)
	
	final val EXP_MOVAVG_LABEL = Vector[Double](
		1.00,1.50,1.50,1.75,2.27,2.74,3.32,3.96,4.58,4.29,3.99,3.55,3.02,2.61,2.11,1.65,1.03
	)
	
	final val WEIGHTED_MOVAVG_LABEL = Vector[Double](
		0.00,0.00,1.70,1.70,2.06,2.72,3.26,3.90,4.58,4.84,4.18,3.64,3.10,2.56,2.14,1.64,1.12,0.48
	)
	
	
		/**
		 * Execution of the scalatest for '''SimpleMovingAveragte''', '''WeightedMovingAverage'''
		 * and '''ExpMovingAverage''' classes
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
		import scala.language.postfixOps
	   
		show(s"$header Evaluation moving averages")
	
			// Extract the partial smoothing functions
		val pfnSMvAve = SimpleMovingAverage[Double](PERIOD) |> 
		val pfnWMvAve = WeightedMovingAverage[Double](WEIGHTS) |>
		val pfnEMvAve = ExpMovingAverage[Double](PERIOD) |>
		
			// Generates the smoothed data
		(for {
			sMvOut <- pfnSMvAve(input)
			eMvOut <- pfnEMvAve(input)
			wMvOut <- pfnWMvAve(input)
		}
		yield {
			assertInt(sMvOut.size, input.size)
			assertXSeries(sMvOut, SIMPLE_MOVAVG_LABEL, 1e-1)
			show(s"Simple moving average ${sMvOut.mkString(",")}")
			
			assertInt(eMvOut.size, input.size)
			assertXSeries(eMvOut, EXP_MOVAVG_LABEL, 1e-1)
			show(s"Exponential moving average ${eMvOut.mkString(",")}")
			
			assertInt(wMvOut.size, input.size)
			assertXSeries(wMvOut, WEIGHTED_MOVAVG_LABEL, 1e-1)
			show(s"Weighted moving average ${wMvOut.mkString(",")}")
			
		}).getOrElse(-1)
		
	}
}


// --------------------------------------  EOF -------------------------------
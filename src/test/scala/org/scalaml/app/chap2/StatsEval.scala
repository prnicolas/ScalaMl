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
import org.scalaml.util.Assertable

		/**
		 * '''Purpose''' Singleton to evaluate basic statistics
		 * @author Patrick Nicolas
		 * @see Scala for Machine Learning Chapter 1 "Getting Started"
		 * @see org.scalaml.app.Eval
		 * @see org.scalaml.util.Assertable
		 */
object StatsEval extends Eval with Assertable {

		/**
		 * Name of the evaluation 
		 */
	val name: String = "StatsEval"
	val assertMsg: String = "StatsEval basic statistics"
	
	private val VALUES = Array[Double](1.0, 2.4, 1.9, 7.3, 3.9, 8.0, 0.5)
	private val MEAN   = 3.571428571
	private val STDDEV = 2.995393288
	private val NORMAL_005 = 0.398443914
	private val NORMAL_095 = 0.254059056
	private val NORMALIZED = Array[Double](
			0.06666, 0.25333, 0.18666, 0.90667, 0.45333, 1.0, 0.0
	)
	private val ZSCORED = Array[Double](
		-0.858461, -0.391076, -0.557999, 1.244768, 0.109692, 1.47846, -1.025384
	)
	private val EPS = 1e-4
	
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
		show(s"$header Basic statistics")
	
			// Create a stats object and compute the normalized and zScored value
		
		val stats = Stats[Double](VALUES)
		val normalized = stats.normalize(0.0, 1.0)
		val zScored = stats.zScore
			
				// Start a sequence of comparison
		compare(MEAN, stats.mean, "Mean:") + 
		compare(STDDEV, stats.stdDev, "Standard Deviation:") +
		{ 
			var sum: Int = 0
			for( i <- 0 until VALUES.size ) 
				yield {sum += compare(NORMALIZED(i), normalized(i), "normalize:")}
			sum
		} +
		compare(NORMALIZED.toVector, normalized, "Least square error") +
		compare(NORMAL_005, Stats.normal(0.05), "Gauss(0.05)") +
		compare(NORMAL_005, Stats.normal(0.05), "Gauss(0.95)") +
		{ 
			var sum: Int = 0
			for( i <- 0 until VALUES.size ) 
				yield {sum += compare(ZSCORED(i), zScored(i), "zScore:")}
			sum
		} 
	}
	
	def display(input: DblVector, normalized: DblVector, zScored: DblVector): Unit = {
	  import org.scalaml.plots._
       val info = Legend(
				"StatsEval", 
				"Statistics: Normalization, zScore, Gauss",
				"Data indices",
				"Values"
			)
			
			val entries = List[(DblVector, String)]( 
       (input, "Input"), 
       (normalized, "0-1 normalized"),
       (zScored, "z-Scored"), 
       (input.map(Stats.gauss(0.0, 1.0, _)), "Gaussian")
			)
     LinePlot.display(entries, info, new LightPlotTheme)
	}
	
	
	private def compare(expected: Double, actual: Double, comment: String): Int = {
		assertDouble(expected, actual, EPS)
		show(s"$comment expected $expected actual $actual")
		
		if( Math.abs(expected - actual) < EPS)  0 else -1
	}
	
	private def compare(expected: DblVector, actual: DblVector, comment: String): Int = {
		val err = expected.zip(actual)./:(0.0)((err, z) => err + (z._1 - z._2)*(z._1 - z._2))
		val lsError = Math.sqrt(err)
		
		show(s"$comment: $lsError")
		if( lsError < EPS)  0 else -1
	}
}

// ----------------------------------- EOF -------------------------------------
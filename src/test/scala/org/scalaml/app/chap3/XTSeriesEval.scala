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
import org.scalaml.app.Eval
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.util.DisplayUtils
import org.scalaml.stats.{XTSeries,Transpose}
import org.apache.log4j.Logger
import org.scalaml.util.FormatUtils
import org.scalaml.util.Assertable
import Transpose._

		/**
		 * Purpose Singleton to evaluate basic operation on time series.
		 * @author Patrick Nicolas
		 * @since 0.98.1 (April 7, 2014)
		 * @see Scala for Machine Learning Chapter 3 ''Data pre-processing'' / Time Series
		 */
object XTSeriesEval extends Eval with Assertable {
  	/**
		 * Name of the evaluation 
		 */
	val name: String = "XTSeriesEval"
	protected val assertMsg: String = "XTSeries evaluation"

	private val EPS = 1e-4
	private val NUM_DATA_POINTS = 10
	
	private val TIMESERIES = Vector[DblArray](
		Array[Double](1.1, 2.5),
		Array[Double](2.5, 0.5),
		Array[Double](3.1, 1.7),
		Array[Double](0.3, 0.5),
		Array[Double](8.2, 12.1),
		Array[Double](7.5, 1.8),
		Array[Double](4.1, 3.0),
		Array[Double](0.8, 11.3),
		Array[Double](9.5, 6.2),
		Array[Double](12.9, 4.8),
		Array[Double](7.0, 3.9)
	)
	private val MEANS = Array[Double](5.181819, 4.39091)
	private val STDDEV = Array[Double](4.10363, 4.00738)
	
	private val NORMALIZED = Vector[DblArray](
		Array[Double](0.063492, 0.17241),
		Array[Double](0.174603, 0.0),
		Array[Double](0.22223, 0.103448),
		Array[Double](0.0, 0.0),
		Array[Double](0.626984, 1.0),
		Array[Double](0.5714285, 0.112068),
		Array[Double](0.3015873, 0.215517),
		Array[Double](0.039682, 0.93103),
		Array[Double](0.730158, 0.491379),
		Array[Double](1.0, 0.370689),
		Array[Double](0.5317460, 0.293103)
	)
	
		/**
		 * Execution of the scalatest for '''XTSeries'''
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
		 * 
		 * @param args array of arguments used in the test
		 */
	override protected def run(args: Array[String]): Int = {
		show(s"$header Generic time series")
		
		val xt = TIMESERIES.toVector
		
		val txt = transpose(xt)
			
			// Compute the statistics for this time series {mean, standard deviation and maximum}
		val stats = XTSeries.statistics(xt).map(x => (x.mean, x.stdDev, x.max))
		stats.foreach{ case (u, v, w) => show(s"mean: $u, stdDev: $v, max: $w") }
				
			// Normalize time series
		val normalized = XTSeries.normalize(xt)
			
		compare(MEANS(0), stats(0)._1, "mean x(0):") +
		compare(STDDEV(0), stats(0)._2, "stddev x(0):") +
		compare(MEANS(1), stats(1)._1, "mean x(1):") +
		compare(STDDEV(1), stats(1)._2, "stddev x(1):") +
		{ 
			var sum: Int = 0
				
			for( i <- 0 until TIMESERIES.size ) 
			yield {
				sum += compare(NORMALIZED(i)(0), normalized.get(i)(0), "normalize: ")
				sum += compare(NORMALIZED(i)(1), normalized.get(i)(1), "normalize: ")
			}
			show(s"Results: $sum")
		}
	}
	
	private def compare(expected: Double, actual: Double, comment: String): Int = {
		show(s"$comment expected $expected actual $actual")
		assertDouble(expected, actual, EPS)
		if( Math.abs(expected - actual) < EPS)  0 else -1
	}
	
	private def compare(predicted: DblArray, expected: DblArray, comment: String): Int = {
		val err = predicted.view.zip(expected.view).map{ case (p, e) => sqr(p-e) }.sum
	
		val lsError = Math.sqrt(err)
		DisplayUtils.show(s"$comment: $lsError", logger)
		if( lsError < EPS)  0 else -1
	}
}

// -----------------------------------  EOF -----------------------------
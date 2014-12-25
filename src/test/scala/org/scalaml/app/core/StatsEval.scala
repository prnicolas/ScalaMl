/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.app.core

import org.scalaml.app.Eval
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.util.DisplayUtils
import org.scalaml.stats.Stats

		/**
		 * <p><b>Purpose</b>Singleton to evaluate basic statistics.</p>
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning
		 */
object StatsEval extends Eval {
	import scala.util.{Try, Success, Failure, Random}
	import org.apache.log4j.Logger
		/**
		 * Name of the evaluation 
		 */
	val name: String = "StatsEval"
	
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
		 * <p>Execution of the scalatest for <b>Stats</b></p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Basic statistics", logger)
	
			// Create a stats object and compute the normalized and zScored value
		Try {
			val stats = Stats[Double](VALUES)
			val normalized = stats.normalize
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
			compare(NORMALIZED, normalized, "Least square error") +
			compare(NORMAL_005, Stats.normal(0.05), "Gauss(0.05)") +
			compare(NORMAL_005, Stats.normal(0.05), "Gauss(0.95)") +
			{ 
				var sum: Int = 0
				for( i <- 0 until VALUES.size ) 
					yield {sum += compare(ZSCORED(i), zScored(i), "zScore:")}
				sum
			} 
		}
		match {
		  case Success(n) => n
		  case Failure(e) => failureHandler(e)
		}
	}
	
	
	private def compare(expected: Double, actual: Double, comment: String): Int = {
		DisplayUtils.show(s"StatsEval $comment expected $expected actual $actual", logger)
		if( Math.abs(expected - actual) < EPS)  0 else -1
	}
	
	private def compare(expected: DblVector, actual: DblVector, comment: String): Int = {
		val err = expected.zip(actual).foldLeft(0.0)((err, z) => err + (z._1 - z._2)*(z._1 - z._2))
		val lsError = Math.sqrt(err)
		DisplayUtils.show(s"StatsEval $comment: $lsError", logger)
		if( lsError < EPS)  0 else -1
	}
}

// ----------------------------------- EOF -------------------------------------
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
package org.scalaml.app.core

import org.scalaml.app.Eval
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.util.DisplayUtils
import org.scalaml.core.XTSeries
import org.apache.log4j.Logger
import org.scalaml.util.FormatUtils

		/**
		 * <p><b>Purpose</b>Singleton to evaluate basic operation on time series.</p>
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning Chapter 3 Data pre-processing / Time Series
		 */
object XTSeriesEval extends Eval {
	import scala.util.{Try, Success, Failure}
  	/**
		 * Name of the evaluation 
		 */
	val name: String = "XTSeriesEval"

	private val EPS = 1e-4
	private val NUM_DATA_POINTS = 10
	
	private val TIMESERIES = Array[Array[Double]](
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
	
	private val NORMALIZED = Array[Array[Double]](
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
		 * <p>Execution of the scalatest for <b>XTSeries</b></p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Generic time series", logger)
		
		val xt = new XTSeries[DblVector]("Test series", TIMESERIES)
		
		Try {
			val txt = XTSeries.transpose(xt)
			
			val stats = XTSeries.statistics(xt).map(x => (x.mean, x.stdDev, x.max))
			stats.foreach(stat => 
					DisplayUtils.show(s"mean: ${stat._1}, stdDev: ${stat._2}, max: ${stat._3}", logger) )
				
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
				sum
			}
		} match {
			case Success(sum) => DisplayUtils.show(s"$name.run results: $sum", logger)
			case Failure(e) => failureHandler(e)
		}
	}
	
	private def compare(expected: Double, actual: Double, comment: String): Int = {
		DisplayUtils.show(s"XTSeriesEval $comment expected $expected actual $actual", logger)
		if( Math.abs(expected - actual) < EPS)  0 else -1
	}
	
	private def compare(expected: DblVector, actual: DblVector, comment: String): Int = {
		val err = expected.zip(actual).foldLeft(0.0)((err, z) => err + (z._1 - z._2)*(z._1 - z._2))
		val lsError = Math.sqrt(err)
		DisplayUtils.show(s"XTSeriesEval $comment: $lsError", logger)
		if( lsError < EPS)  0 else -1
	}
}

// -----------------------------------  EOF -----------------------------
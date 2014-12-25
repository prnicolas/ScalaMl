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
package org.scalaml.app.chap6

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.YahooFinancials
import org.scalaml.supervised.regression.linear.SingleLinearRegression
import org.scalaml.util.{FormatUtils, DisplayUtils}
import org.scalaml.core.XTSeries
import org.scalaml.app.Eval

		/**
		 * <p><b>Purpose:</b> Singleton to evaluate the single variate linear regression.</p>
		 * @author Patrick Nicolas
	   * @note: Scala for Machine Learning Chapter 6: Regression and regularization / One-variate 
	   * linear regression
		 */ 
object SingleLinearRegressionEval extends Eval {
	import scala.util.{Try, Success, Failure}
	import org.apache.log4j.Logger
	import YahooFinancials._, XTSeries._
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "SingleLinearRegressionEval"

	private val path = "resources/data/chap6/CU.csv"

		/**
		 * <p>Execution of the scalatest for <b>SingleLinearRegression</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int =  {
		DisplayUtils.show(s"$header Evaluation of single variate linear regression", logger)
		
		Try {
			val price = DataSource(path, false, true, 1) |> adjClose
			val xy = price.zipWithIndex.map(x => (x._2.toDouble, x._1.toDouble))
			val linRegr = SingleLinearRegression(xy)
		    
			val slope = linRegr.slope
			val intercept = linRegr.intercept
			assert(slope != None, s"Simple regression model is undefined")
			
			val slope_str = FormatUtils.format(slope.get, "y= ", FormatUtils.ShortFormat)
			val intercept_str = FormatUtils.format(intercept.get, " ", FormatUtils.ShortFormat)
			DisplayUtils.show(s"$name Linear regression: $slope_str.x + $intercept_str", logger)
			DisplayUtils.show(s"$name validation: ${lsError(xy.toArray, slope.get, intercept.get)}", 
						logger)  
		}
		match {
			case Success(n) => n
			case Failure(e) => failureHandler(e)
		}
	}
	
	
	private def lsError(xyt: Array[(Double, Double)], slope: Double, intercept: Double): Double = {
		val error = xyt.foldLeft(0.0)((err, xy) => {
			val diff = xy._2 - slope*xy._1 - intercept
			err + diff*diff  
		})
		Math.sqrt(error/xyt.size)
	}
}

// ----------------------------  EOF ----------------------------------
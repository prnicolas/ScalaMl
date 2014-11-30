/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.app.chap6

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.YahooFinancials
import org.scalaml.supervised.regression.linear.SingleLinearRegression
import YahooFinancials._
import org.scalaml.core.Types.ScalaMl
import org.apache.log4j.Logger
import org.scalaml.util.Display
import org.scalaml.core.XTSeries
import scala.util.{Try, Success, Failure}
import XTSeries._
import org.scalaml.app.Eval



		/**
		 * <p>Singleton to evaluate the single variate linear regression.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since April 23, 2014
	     * @note: Scala for Machine Learning Chapter 6: Regression and regularization/One-variate linear regression
		 */
object SingleLinearRegressionEval extends Eval {
	val name: String = "SingleLinearRegressionEval"
	val maxExecutionTime: Int = 7000
	
	private val path = "resources/data/chap6/CU.csv"
	private val logger = Logger.getLogger(name)

		/**
		 * <p>Execution of the scalatest for <b>SingleLinearRegression</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int =  {
		Display.show(s"\n\n *****  test#${Eval.testCount} $name Evaluation of single variate linear regression", logger)
		
		Try {
			val price = DataSource(path, false, true, 1) |> adjClose
			val xy = price.zipWithIndex.map(x => (x._2.toDouble, x._1.toDouble))
			val linRegr = SingleLinearRegression(xy)
		    
			val slope = linRegr.slope
			val intercept = linRegr.intercept
			if( slope != None ) {
				Display.show(s"$name Linear regression: ${ScalaMl.toString(slope.get, "y= ", true)}.x + ${ScalaMl.toString(intercept.get, "", true)}", logger)
				Display.show(s"$name validation: ${lsError(xy.toArray, slope.get, intercept.get)}", logger)
			}
			else
				Display.error(s"$name run failed compute slope", logger)
		    	  
		}match {
			case Success(n) => n
			case Failure(e) => Display.error(s"$name failed to be build a model", logger, e)
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
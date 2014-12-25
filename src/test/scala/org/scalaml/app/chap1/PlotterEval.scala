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
package org.scalaml.app.chap1

import org.scalaml.core.Types.ScalaMl.{ DblVector, XYTSeries}
import org.scalaml.stats.Stats
import org.scalaml.plots._
import org.scalaml.util.DisplayUtils
import org.scalaml.app.Eval

		/**
		 * <p><b>Purpose</b>Singleton to evaluate the different categories of plot
		 * (line, scatter.)  used inthe Scala for Machine learning</p>
		 * 
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning Chapter 1
		 */
object PlotterEval extends Eval {
	import scala.util.{Try, Success, Failure}
	import scala.io.Source
	import org.apache.log4j.Logger
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "PlotterEval"

	private val CSV_DELIM = ","
	private val PRICE_COLUMN_INDEX = 6
	private val OPEN_COLUMN_INDEX = 1
	private val VOL_COLUMN_INDEX = 5
	private val HIGH_INDEX = 2
	private val LOW_INDEX = 3
	private val pathName = "resources/data/chap1/CSCO.csv"
    
 
		/**
		 * <p>Execution of the scalatest for <p>LinePlot</p> and <p>ScatterPlot</p> classes. 
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Evaluation of JFreeChart library", logger)
		
		Try {
			val src = Source.fromFile(pathName)
			val fields = src.getLines.map( _.split(CSV_DELIM)).toArray	   
			
			val cols = fields.drop(1)
			val volatility: DblVector = Stats[Double](cols.map(f => f(HIGH_INDEX).toDouble - f(LOW_INDEX).toDouble))
					.normalize
			val normVolume =  Stats[Double](cols.map( _(VOL_COLUMN_INDEX).toDouble) ).normalize
			val volatility_volume: XYTSeries = volatility.zip(normVolume)
			
			DisplayUtils.show(s"$name Line plot for CSCO stock normalized volume", logger)
			val labels1 = List[String](
				name, s"$name: CSCO 2012-2013 Stock volume", "Volume", "r"
			)
			LinePlot.display(normVolume, labels1, new LightPlotTheme)

			DisplayUtils.show(s"$name Line plot for CSCO stock volatility", logger)
			val labels2 = List[String](
				name, s"$name: CSCO 2012-2013 Stock Volatility", "Volatility", "r"
			)
			ScatterPlot.display(volatility, labels2, new BlackPlotTheme)
		    	
			DisplayUtils.show(s"$name Scatter plot CSCO stock volatility vs. volume", logger)
			val labels3 = List[String](
				name, s"$name: CSCO 2012-2013 Stock volatility vs. volume", "Volatility vs. Volume", "r"
			)
			ScatterPlot.display(volatility_volume, labels3, new LightPlotTheme)
			src.close
		} 
		match {
			case Success(n) => DisplayUtils.show(s"$name Test completed", logger)
			case Failure(e) => failureHandler(e)
		}
	}
}

// ---------------------------------------  EOF ----------------------------------------------
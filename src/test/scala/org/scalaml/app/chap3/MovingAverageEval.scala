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
package org.scalaml.app.chap3

import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.core.XTSeries
import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import org.scalaml.util.{FormatUtils, DisplayUtils}
import org.scalaml.filtering.{SimpleMovingAverage, WeightedMovingAverage, ExpMovingAverage}
import org.scalaml.app.Eval
import XTSeries.DblSeries

		/**
		 * <p><b>Purpose:</b>Singleton used to test the moving average algorithms
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning Chapter 3 Data Pre-processing / Moving averages
		 */
object MovingAveragesEval extends FilteringEval {
	import scala.util.{Try, Success, Failure}
	import org.apache.log4j.Logger
	import YahooFinancials._
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "MovingAveragesEval"
    
	private val START_DISPLAY = 32
	private val WINDOW_DISPLAY = 64
	private val RESOURCE_PATH = "resources/data/chap3/"
	private val OUTPUT_PATH = "output/chap3/mvaverage"
	  
		/**
		 * <p>Execution of the scalatest for <b>SimpleMovingAveragte</b>, <b>WeightedMovingAverage</b>
		 * and <b>ExpMovingAverage</b> classes
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	override def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Evaluation moving averages", logger)
		if(args.size > 1) {
			val symbol = args(0)
			val p = args(1).toInt
			val p_2 = p >>1
			val w = Array.tabulate(p)(n => if( n == p_2) 1.0 else 1.0/(Math.abs(n -p_2)+1))
			val weights: DblVector = w.map { _ / w.sum }
			DisplayUtils.show(FormatUtils.format(weights, "Weights", FormatUtils.ShortFormat), logger)
	     
			val dataSource = DataSource(RESOURCE_PATH + symbol + ".csv", false)
			Try {
				val price = dataSource |> YahooFinancials.adjClose
				val sMvAve = SimpleMovingAverage[Double](p)  
				val wMvAve = WeightedMovingAverage[Double](weights)
				val eMvAve = ExpMovingAverage[Double](p)
		
				val dataSink = DataSink[Double](OUTPUT_PATH + p.toString + ".csv")
				val results = price :: 
										sMvAve.|>(price) :: 
										eMvAve.|>(price) :: 
										wMvAve.|>(price) :: 
										List[DblSeries]()
	
				dataSink |> results
				DisplayUtils.show(s"$name Results for [$START_DISPLAY, $WINDOW_DISPLAY] values", logger)
				results.foreach(ts => {
					val displayedValues = ts.toArray.drop(START_DISPLAY).take(WINDOW_DISPLAY)
					DisplayUtils.show(FormatUtils.format(displayedValues, "X", FormatUtils.ShortFormat), logger)
				})
				
				display(List[DblSeries](results(0), results(1)), "Simple Moving Average")
				display(List[DblSeries](results(0), results(2)), "Exponential Moving Average")
				display(List[DblSeries](results(0), results(3)), "Weighted Moving Average")
			}
			match {
				case Success(n) => n
				case Failure(e) => failureHandler(e)
			}
		}
		else 
			DisplayUtils.error(s"$name Incorrect arguments for command line", logger)
	}
	
	private def display(results: List[DblSeries], label: String): Int = {
		import org.scalaml.plots.{LinePlot, LightPlotTheme}
		
		val labels = List[String]( 
			name, label, "Trading sessions", "Stock price"
		)
		val dataPoints: Array[(DblVector, String)] = results.map(_.toArray).toArray.zip(labels)
		LinePlot.display(dataPoints.toList, labels, new LightPlotTheme)
		0
	}
}

// --------------------------------------  EOF -------------------------------
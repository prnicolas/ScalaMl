/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.3
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
		/**
		 * Maximum duration allowed for the execution of the evaluation
		 */
    val maxExecutionTime: Int = 25000
    
	private val logger = Logger.getLogger(name)
	private val NUMBER_DISPLAYED_VALUES = 128

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
			val weights: DblVector = w map { _ / w.sum }
			DisplayUtils.show(FormatUtils.format(weights, "Weights", FormatUtils.ShortFormat), logger)
	     
			val dataSource = DataSource("resources/data/chap3/" + symbol + ".csv", false)
			Try {
				val price = dataSource |> YahooFinancials.adjClose
				val sMvAve = SimpleMovingAverage[Double](p)  
				val wMvAve = WeightedMovingAverage[Double](weights)
				val eMvAve = ExpMovingAverage[Double](p)
		
				val dataSink = DataSink[Double]("output/chap3/mvaverage" + p.toString + ".csv")
				val results = price :: sMvAve.|>(price) :: 
										eMvAve.|>(price) :: 
										wMvAve.|>(price) :: 
										List[DblSeries]()
	
				dataSink |> results
				DisplayUtils.show(s"$name Results for the first $NUMBER_DISPLAYED_VALUES values", logger)
				results.foreach(ts => {
					val displayedValues = ts.toArray.take(NUMBER_DISPLAYED_VALUES)
					DisplayUtils.show(FormatUtils.format(displayedValues, "X", FormatUtils.ShortFormat), logger)
				})
				
				display(List[DblSeries](results(0), results(1)), 
						List[String]("Stock price", "Simple Moving Average"))
				display(List[DblSeries](results(0), results(2)), 
						List[String]("Stock price", "Exponential Moving Average"))
				display(List[DblSeries](results(0), results(3)), 
						List[String]("Stock price", "Weighted Moving Average"))
			}
			match {
				case Success(n) => n
				case Failure(e) => DisplayUtils.error(s"$name One of the Moving averages failed", logger, e)
			}
		}
		else 
			DisplayUtils.error(s"$name Incorrect arguments for command line", logger)
	}
	
	private def display(results: List[DblSeries], labels: List[String]): Int = {
		import org.scalaml.plots.{LinePlot, LightPlotTheme, BlackPlotTheme}
		
		val plot = new LinePlot(("Moving Averages", "Trading sessions", "Stock price"), 
				new LightPlotTheme)
		val dataPoints: Array[(DblVector, String)] = results.map(_.toArray).toArray.zip(labels)
		plot.display(dataPoints.toList, 340, 270)
		1
	}
}

// --------------------------------------  EOF -------------------------------
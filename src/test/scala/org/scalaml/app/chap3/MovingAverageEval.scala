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
import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import org.scalaml.util.{FormatUtils, DisplayUtils, LoggingUtils}
import org.scalaml.filtering.movaverage._
import org.scalaml.app.Eval
import LoggingUtils._, YahooFinancials._, XTSeries._, DisplayUtils._, FormatUtils._

		/**
		 * '''Purpose:'''Singleton used to test the moving average algorithms
		 * @author Patrick Nicolas
		 * @since 0.98  (March 7, 2014)
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 3 "Data Pre-processing" / Moving averages
		 * @see org.scalaml.filtering.MovingAverage
		 */
object MovingAverageEval extends FilteringEval {
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "MovingAveragesEval"
    
	private val START_DISPLAY = 32
	private val WINDOW_DISPLAY = 64
	private val RESOURCE_PATH = "resources/data/chap3/"
	private val OUTPUT_PATH = "output/chap3/mvaverage"
	  
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
		
		if(args.size > 1) {
			val symbol = args(0)
			val p = args(1).toInt
			val p_2 = p >>1
			val w = Array.tabulate(p)(n => if( n == p_2) 1.0 else 1.0/(Math.abs(n -p_2)+1))
			val sum = w.sum
			val weights: DblArray = w.map { _ / sum }
			

			val dataSrc = DataSource(s"$RESOURCE_PATH$symbol.csv", false)
			  
					// Extract the partial functions associated to the simple, weighted and
					// exponential moving averages
			val pfnSMvAve = SimpleMovingAverage[Double](p) |> 
			val pfnWMvAve = WeightedMovingAverage[Double](weights) |>
			val pfnEMvAve = ExpMovingAverage[Double](p) |>
		
			val results: Try[Int] = for {
				price <- dataSrc.get(adjClose)
					// Executes the simple moving average
				if pfnSMvAve.isDefinedAt( price)
					sMvOut <- pfnSMvAve(price)
					
					// Executes the exponential moving average
				if pfnEMvAve.isDefinedAt(price)
					eMvOut <- pfnEMvAve(price)
				
					// Executes the weighted moving average
				if pfnWMvAve.isDefinedAt(price)
						wMvOut <- pfnWMvAve(price)
			}
			yield {
				val dataSink = DataSink[Double](s"$OUTPUT_PATH$p.csv")
				
					// Collect the results to be saved into a data sink (CSV file)
				val results = List[DblVector](price, sMvOut, eMvOut, wMvOut)
				dataSink |> results
				  
				show(s"Results for [$START_DISPLAY, $WINDOW_DISPLAY] values")
				
				results.map( window(_)).map( display(_)) 
				display(price, sMvOut, "Simple Moving Average")
				display(price, eMvOut, "Exponential Moving Average")
				display(price, wMvOut, "Weighted Moving Average")
			}
			results.get
		}
		
		else 
			error(s"Incorrect args. command line required 2")
	}
	

	private def window(series: DblVector): DblVector = 
			series.drop(START_DISPLAY).take(WINDOW_DISPLAY)

	private def display(values: DblVector): Unit = show(format(values, "X", SHORT))
	
	private def display(price: DblVector, smoothed: DblVector, label: String): Int = {
		import org.scalaml.plots.{LinePlot, LightPlotTheme, Legend}
		
		val labels = Legend( 
			name, label, "Trading sessions", "Stock price"
		)
		
		val dataPoints = List[DblVector](price,smoothed).map(_.toVector).zip(labels.toList)
		LinePlot.display(dataPoints.toList, labels, new LightPlotTheme)
		0
	}
}


// --------------------------------------  EOF -------------------------------
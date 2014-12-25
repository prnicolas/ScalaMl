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

import org.scalaml.plots._
import org.scalaml.stats.Stats
import org.scalaml.trading.{Signal, YahooFinancials}
import org.scalaml.core.Types
import org.scalaml.core.Types.ScalaMl
import org.scalaml.supervised.regression.logistic.LogBinRegression
import org.scalaml.util.DisplayUtils
import org.scalaml.app.Eval

		/**
		 * <p><b>Purpose</b>Singleton to evaluate a simple implementation of a 
		 * two class logistic regression for a single variable</p>
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning chapter 1
		 */
object LogBinRegressionEval extends Eval {
	import java.awt.Color	
	import scala.io.Source
	import scala.util.{Try, Success,Failure}
	import org.apache.log4j.Logger
	import YahooFinancials._, Signal._,  ScalaMl._
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "LogBinRegressionEval"
		
	private val NITERS = 300
	private val EPS = 0.02
	private val ETA = 0.000002
	private val path_training = "resources/data/chap1/CSCO.csv"
	private val path_test = "resources/data/chap1/CSCO2.csv"

		/**
		 * <p>Execution of the scalatest for <b>LogBinRegression</b> class. This method is invoked by the 
		 * actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Loading history Cisco stock for logistic regression", logger)
		
			// Uses the for-comprehension loop to process sequence of
			// nested options...
		(for {
			volatilityVolume <- load(path_training)	// extract volatility relative to volume
			logit <- newLogit(volatilityVolume)			// Generate the logistic regression
			test <- load(path_test)									// Load the test data
			topCategory <- logit.classify(test(0)) 	// classify first data point
			topCategory2 <- logit.classify(test(1)) // classify second data point
		} 
		yield {
			val result = s"$name test result ${topCategory.toString}\n" +
					s"$name test result ${topCategory2.toString}"
			DisplayUtils.show(s"$result", logger)
		}).getOrElse(-1)
	}
	
	
	private def newLogit(volatilityVolume: XYTSeries): Option[LogBinRegression] = {
		DisplayUtils.show(s"$name DisplayUtils of stock volatility vs. volume", logger)
		display(volatilityVolume)
	    	
		val labels = volatilityVolume.zip(volatilityVolume.map(x => 
			if(x._1 > 0.2 && x._2 > 0.45) 1.0 else 0.0 ))
		val logit = new LogBinRegression(labels, NITERS, ETA, EPS)  
		DisplayUtils.show(s"$name Loading history Cisco stock for testing", logger)
		Some(logit)
	}
			/**
		 * Method to load and normalize the volume and volatility of a stock.
		 */
	private def load(fileName: String): Option[XYTSeries] = {
	require(fileName != Types.nullString, 
			"LogBinRegressionEval.load Cannot load data from undefined fileName")
			
		Try {
			val src =  Source.fromFile(fileName)
			val fields = src.getLines.map( _.split(CSV_DELIM)).toArray
			val cols = fields.drop(1)
			val data = transform(cols)
			src.close
			data
		} 
		match {
			case Success(xySeries) => Some(xySeries)
			case Failure(e) => DisplayUtils.none(s"$name.load", logger, e)
		}
	}
    
	private def transform(cols: Array[Array[String]]): XYTSeries = {
		val volatility  = Stats[Double](cols.map(YahooFinancials.volatility)).normalize
		val volume =  Stats[Double](cols.map(YahooFinancials.volume) ).normalize
		volatility.zip(volume)
	}
    
		/**
		 * Method to display a time series on a line plot
		 */
	private def display(volatilityVolume: XYTSeries): Unit = {
		if( DisplayUtils.isChart ) {
			val labels = List[String](
				"LogBinRegressionEval", 
				"CSCO 2012-13 stock price session volatiity",
				"Normalized session volatility",
				"Normalized session Volume"
			)
			ScatterPlot.display(volatilityVolume, labels, new BlackPlotTheme)
		}
	}
}

// --------------------  EOF --------------------------------------
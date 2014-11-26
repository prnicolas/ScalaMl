/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.app.chap1

import scala.io.Source
import java.awt.Color
import org.scalaml.plots._
import org.scalaml.stats.Stats
import org.scalaml.trading.{Signal, YahooFinancials}
import org.scalaml.core.Types.ScalaMl
import Signal._
import ScalaMl._
import org.scalaml.supervised.regression.logistic.LogBinRegression
import scala.util.{Try, Success,Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display
import org.scalaml.app.Eval

		/**
		 * <p><b>Purpose</b>Singleton to evaluate a simple implementation of a 
		 * two class logistic regression for a single variable</p>
		 * @author Patrick Nicolas
		 * @since December 22, 2013
		 * @note Scala for Machine Learning chapter 1
		 */
object LogBinRegressionEval extends Eval {
	import YahooFinancials._
	val name: String = "LogBinRegressionEval"
	
	private val NITERS = 300
	private val EPS = 0.02
	private val ETA = 0.000002
	private val path_training = "resources/data/chap1/CSCO.csv"
	private val path_test = "resources/data/chap1/CSCO2.csv"
	private val logger = Logger.getLogger(name)
    		

		/**
		 * <p>Execution of the scalatest for <p>LogBinRegression</p> class. This method is invoked by the 
		 * actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		Display.show(s"\n** test#${Eval.testCount} $name Loading history Cisco stock for training logistic regression", logger)
		
		load(path_training) match {
			case Some(volatilityVolume) => {
				Display.show(s"$name Display of stock volatility vs. volume", logger)
				display(volatilityVolume)
	    	
				val labels = volatilityVolume.zip(volatilityVolume.map(x => 
					if(x._1 > 0.2 && x._2 > 0.45) 1.0 else 0.0 ))
				val logit = new LogBinRegression(labels, NITERS, ETA, EPS)
	    	  
				Display.show(s"$name Loading history Cisco stock for testing", logger)
				load(path_test) match {
					case Some(test) =>{
						logit.classify(test(0)) match {
							case Some(topCategory) => Display.show(s"$name test result ${topCategory.toString}", logger)
							case None => Display.error(s"$name Failed to classify", logger)
						}
						logit.classify(test(1)) match {
							case Some(topCategory) => Display.show(s"$name test result ${topCategory.toString}", logger)
							case None => Display.error(s"$name Failed to classify", logger)
						}
					}	
					case None => Display.error(s"$name Could not load training set for $path_test", logger)
				}
			}
			case None => Display.error(s"$name  Could not load test set for $path_training", logger)
		}
    }
	
			/**
		 * Method to load and normalize the volume and volatility of a stock.
		 */
	private def load(fileName: String): Option[XYTSeries] = {
	require(fileName != null, "LogBinRegressionEval.load Cannot load data from undefined fileName")
		Try {
			val src =  Source.fromFile(fileName)
			val fields = src.getLines.map( _.split(CSV_DELIM)).toArray
			val cols = fields.drop(1)
			val data = transform(cols)
			src.close
			data
		} match  {
			case Success(xySeries) => Some(xySeries)
			case Failure(e) => Display.error("LogBinRegressionEval.load", logger, e); None
		}
	}
    
	private def transform(cols: Array[Array[String]]): XYTSeries = {
		val volatility  = Stats[Double](cols.map(YahooFinancials.volatility)).normalize
		val volume =  Stats[Double](cols.map(YahooFinancials.volume) ).normalize
		volatility.zip(volume)
	}
    
		/**
		 * Method to display a time series
		 */
	private def display(volatilityVolume: XYTSeries): Unit = {
		require(volatilityVolume != null && volatilityVolume.size > 0, "LogBinRegressionEval.displayCannot display an undefined time series")
       
		val plotter = new ScatterPlot(("CSCO 2012-13 stock price session volatiity", "Normalized session volatility", "Normalized session Volume"), 
				new BlackPlotTheme)
		plotter.display(volatilityVolume, 250, 340)
	}
}


// --------------------  EOF --------------------------------------
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
package org.scalaml.app.chap1


import java.awt.Color
import scala.util.{Try, Success, Failure}
import scala.io.Source
import org.apache.log4j.Logger
import org.scalaml.plots._
import org.scalaml.stats.{Stats, MinMaxVector}
import org.scalaml.trading.{Signal, YahooFinancials}
import org.scalaml.core.Types
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.supervised.regression.logistic.LogBinRegression
import org.scalaml.util.{DisplayUtils,  LoggingUtils}
import org.scalaml.app.Eval
import LoggingUtils._, YahooFinancials._
import org.scalaml.stats.XTSeries._

		/**
		 * '''Purpose'''Singleton to evaluate a simple implementation of a 
		 * two class logistic regression for a single variable
		 * @author Patrick Nicolas
		 * @since 0.97  November 11, 2013
		 * @version 0.98.2
		 * @see Scala for Machine Learning chapter 1 "Getting Started" / Let's kick the tires
		 */
object LogBinRegressionEval extends Eval {
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "LogBinRegressionEval"
		
	private val NITERS = 4000
	private val EPS = 5e-4
	private val ETA = 0.005
	private val path_training = "resources/data/chap1/CSCO.csv"
	private val path_test = "resources/data/chap1/CSCO2.csv"

	
	type Labels = (XVSeries[Double], XSeries[Double])
	  
		/**
		 * Execution of the scalatest for '''LogBinRegression''' class. This method is invoked by the 
		 * actor-based test framework function, ScalaMlTest.evaluate.
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
	override protected def run(args: Array[String]): Int =  {
		show(s"$header binomial logistic regression")
		
			// Uses the for-comprehension loop to implement the computational data flow
		(for {
				// Data preparation and training
			volatilityVolP <- load(path_training)								// extract volatility relative to volume
			minMaxVec <- Try(new MinMaxVector(volatilityVolP._1))		// create a MinMax normalizer
			normVolatilityVol <- Try(minMaxVec.normalize(0.0, 1.0))	// normalize over [0, 1]
			classifier <- logRegr(normVolatilityVol, volatilityVolP._2)		// Generate the model
			
				// Test logistic regression
			testValues <- load(path_test)														// Load the test data
			normTestValue0 <- minMaxVec.normalize(testValues._1(0))	// normalize test values
			class0 <- classifier.classify(normTestValue0) 		
			normTestValue1 <- minMaxVec.normalize(testValues._1(1))	// classify first data point
			class1 <- classifier.classify(normTestValue1)						// classify second data point
		} 
		yield {
				// Display the labeled data: Stock price
			val stockPrice = normalize(volatilityVolP._2)
			displayLabel(stockPrice.get)
			
				// Retrieve the model parameters (weigths)
			val modelStr = classifier.model.toString
			val result = s"""${toString(testValues._1(0),normTestValue0, class0)}
					|\n${toString(testValues._1(1),normTestValue1, class1)}""".stripMargin   
			
			show(s"$modelStr\n$result")
		}).get
	}

	
	private def toString(testValue: DblArray, normValue: DblArray, res: (Int, Double)): String = 
		s"input ${testValue.mkString(",")} normalized ${normValue.mkString(",")} class $res"
	
	
		/**
		 * This method display the normalized input data in a Scatter plot
		 * generates the labels and instantiate the binomial logistic regression
		 */
	private def logRegr(labels: Labels): Try[LogBinRegression] = Try {
		display(labels._1.map( x => (x(0), x(1))))
		
			// Extract labels and create two classes
		val normLabels = normalize(labels._2).get


			// Generate a vector of type (DblPair, Double)
		new LogBinRegression(labels._1, normLabels, NITERS, ETA, EPS)  
	}
	
		/**
		 * Method to load and normalize the volume and volatility of a stock.
		 */
	private def load(fileName: String): Try[Labels] =  {
		val src =  Source.fromFile(fileName)
		val data = extract(src.getLines.map( _.split(",")).drop(1))
		src.close
		data
	}

	private def extract(cols: Iterator[Array[String]]): Try[Labels] = Try {
		val features = Array[YahooFinancials](LOW, HIGH, VOLUME, OPEN, ADJ_CLOSE)
		val conversion = toDblArray(features)

		cols.map( conversion(_))
				.toVector
				.map(x => (Array[Double](1.0 - x(0)/x(1), x(2)), x(4)/x(3)- 1.0)).unzip
	}

		/**
		 * Method to display a time series two features: relative volatility
		 * and volume 
		 */
	private def display(volatilityVolume: Vector[DblPair]): Unit = {
		if( DisplayUtils.isChart ) {
			val info = Legend(
				"LogBinRegressionEval", 
				"LogBinRegression: CSCO 2012-13: Model features",
				"Normalized session volatility",
				"Normalized session Volume"
			)
			ScatterPlot.display(volatilityVolume, info, new BlackPlotTheme)
		}
	}
		
			/**
		 * Method to display the label data: Stock price
		 */
	private def displayLabel(price: XSeries[Double]): Unit = {
		if( DisplayUtils.isChart ) {
			val info = Legend(
				"LogBinRegressionEval", 
				"CSCO 2012-13: Training label",
				"Trading sessions",
				"Normalized stock price variation"
			)
			LinePlot.display(price, info, new BlackPlotTheme)
		}
	}
}

// --------------------  EOF --------------------------------------
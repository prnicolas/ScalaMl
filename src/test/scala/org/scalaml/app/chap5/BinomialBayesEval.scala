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
package org.scalaml.app.chap5

import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.language.postfixOps
import org.apache.log4j.Logger

import org.scalaml.trading.YahooFinancials
import org.scalaml.core.Types.ScalaMl
import org.scalaml.stats.{Difference, XTSeries}
import org.scalaml.workflow.data.{DataSource,DocumentsSource}
import org.scalaml.filtering.movaverage.SimpleMovingAverage
import org.scalaml.supervised.bayes.NaiveBayes
import org.scalaml.validation.OneFoldXValidation
import org.scalaml.util.DisplayUtils
import org.scalaml.app.Eval
import YahooFinancials._, ScalaMl._, SimpleMovingAverage._, XTSeries._, Difference._, XTSeries.Transpose._

		/**
		 * Generic trait that implements the extraction of data associated to a stock
		 * or ETF ticker symbol, located into a directory
		 * @author Patrick Nicolas
		 * @note Scala for Machine learning Chapter 5 Naive Bayes Models
		 */
trait BayesEval extends Eval {
	protected val path = "resources/data/chap5/"

	  
	override protected def run(args: Array[String]): Int

		/**
		 * Extractor for data in files associated to a list of ticker symbols
		 */
	protected val extractor = toDouble(CLOSE) :: 
								ratio (HIGH, LOW) :: 
								toDouble(VOLUME) ::
								List[Array[String] =>Double]()
	protected def symbolFiles = DataSource.listSymbolFiles(path)
}

		/**
		 * '''Purpose''' Singleton to evaluate the Binomial Naive Bayes classifier.
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see org.scalaml.supervised.bayes
		 * @see Scala for Machine learning Chapter 5 ''Naive Bayes Models''
		 * @note Organize the evaluation of the binomial Naive Bayes as a pipeline of data 
		 * transformations
		 */
object BinomialBayesEval extends BayesEval {
	type Result = List[(List[Int], Int)]
		/**
		 * Name of the evaluation 
		 */
	val name: String = "BinomialBayesEval"
	  
	  
	type Output = List[Array[Int]]
	  
		/** 
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
		 */
	override protected def run(args: Array[String]): Int = {
		require( args.length > 2, s"$name.run run argument found ${args.length} required > 2")
		
		show(s"$header Binomial Naive Bayes for ${args(0)} stock")	
		
		val symbol = s"${args(0)}.csv"
		val trainRatio = args(1).toDouble
		val period = args(2).toInt
		val description = s"symbol: ${args(0)} smoothing period:$period"
		
			// Partial function for the simple moving average
		val pfnMv = SimpleMovingAverage[Double](period, false) |>
		
			// Partial function for the extraction of data 
		val pfnSrc = DataSource(symbol, path, true, 1) |>
		
			// Binary quantization of the difference between two values in a pair
		val delta = (x: DblPair) => if(x._1 > x._2) 1 else 0
	
			// Compute the difference between the value of the features of an observation, obs(n) and
			// its simple moving average sm(i) for all i.
		def computeDeltas(obs: XVSeries[Double]): Try[(XVSeries[Double], Vector[Array[Int]])] = Try {
		
				// Simple moving average value
			val sm = obs.map(_.toVector).map( pfnMv(_).get.toArray)
				// Observations beyond the initial period of observations
			val x = obs.map(_.drop(period-1) )
				// Compute the difference
			(x, x.zip(sm).map{ case(_x, y) => _x.zip(y).map( delta(_)) }) 
		}

		
		(for {
				// Extracts the observations
			obs <- pfnSrc(extractor)
			
				// Compute the difference between the value of a feature and its moving average
			(x, deltas) <- computeDeltas(obs)
			
				// Extract the labels or expected outcome using the XTSeries.difference
			expected <- Try { difference(x.head.toVector, diffInt) }
			
				// Extract the time series of features using XTSeries.transpose
			features <- Try { transpose(deltas) }
			
				// Applies a one fold validation of the features and expected values using
				// a ratio of the size of the training set, trainRatio
			labeledData <- OneFoldXValidation[Int](features.drop(1), expected, trainRatio)
			
				// Constructs the Naive Bayes model
			nb <- NaiveBayes[Int](1.0, labeledData.trainingSet)
			
				// Computes the F1 score
			f1Score <- nb.validate(labeledData.validationSet)
		}
		yield {	
			show("Time series of observations\n")
			show(obs.take(64).map(_.mkString("\n")).mkString("\n"))

			show("Model input features\n")
			show(features.take(64).map(_.mkString(",")).mkString("\n"))
			
			val labels = Array[String](
					"price/ave price",
					"volatility/ave. volatility",
					"volume/ave. volume"
			)
			display(x, labels)
			show(s"\nModel: ${nb.toString(labels)}\nF1 score: $f1Score")
		}).getOrElse(-1)
	}
	
	private def display(data: XVSeries[Double], labels: Array[String]): Unit = {
		import org.scalaml.plots.{Legend, LinePlot, LightPlotTheme}
		val legend = new Legend("Features", "Binomial Naive Bayes classification", "Trading sessions", "Metrics")
		
		LinePlot.display( data.map(_.toVector).zip(labels).toList, legend, new LightPlotTheme)
	}
}

// -----------------------------  EOF ------------------------------------
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
package org.scalaml.app.chap6

import scala.util.Try
import scala.language.postfixOps
		
import org.apache.log4j.Logger

import org.scalaml.trading.YahooFinancials
import org.scalaml.stats.{Difference, XTSeries}
import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.Types.emptyString
import org.scalaml.workflow.data.{DataSink, DataSource}
import org.scalaml.supervised.regression.linear.MultiLinearRegression
import org.scalaml.filtering.movaverage.SimpleMovingAverage
import org.scalaml.util.{FormatUtils, DisplayUtils, LoggingUtils}
import org.scalaml.supervised.regression.Regression._
import org.scalaml.app.Eval
import LoggingUtils._, XTSeries._, Difference._, YahooFinancials._, MultiLinearRegression._, FormatUtils._, ScalaMl._



		/**
		 * '''Purpose:''' Singleton to test the multi-variate least squares regression. 
		 * The evaluation consists of extracting trends from noisy data using the multi-nomial
		 * linear regression
		 * 
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see org.scalaml.supervised.regression.linear
		 * @see Scala for Machine Learning Chapter 6 ''Regression and regularization'' / Ordinary least 
		 * squares regression 
		 */
object MultiLinearRegrTrendsEval extends Eval {
	import scala.collection.mutable.ListBuffer	


	final val path = "resources/data/chap6/CU.csv"
	final val output = "output/chap6/CU_output.csv"
		/**
		 * Name of the evaluation 
		 */
	val name: String = "MultiLinearRegrTrendsEval"
 
		/**
		 * Execution of the scalatest for '''MultiLinearRegression''' class.
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
		show(s"$header Ordinary least squares regression TRENDING")

		val src = DataSource(path, true, true, 1)
		val pfnSink = DataSink[Double](output) |>
			
		(for {
			prices <- src get YahooFinancials.adjClose				// extract the stock price
			volatility <- src get YahooFinancials.volatility 	// extract the stock volatility
			volume <- src get YahooFinancials.volume					// extract trading volume
			(features, expected) <- differentialData(volatility, volume, prices, diffDouble)
			regression <- MultiLinearRegression[Double](features, expected)
		}
		yield {
		  
		  if( regression.isModel) {
					// Save data into file
				pfnSink(expected :: volatility :: volume :: List[DblVector]())
	
					// Use the model created through training
	
				val weightsStr = regression.weights.get.view.zipWithIndex.map{ case (w, n) => s"${w}${n}"} 
				val trend = features.map( dot(_, regression.weights.get) ) //w(0) + z(0)*w(1) +z(1)*w(2))
	
				display(expected, trend)
				show( s"""Multi-regression weights\n ${weightsStr.mkString(" ")}
						| \nDelta:\n${expected.mkString(",")}\nTrend: ${trend.mkString(",")}""".stripMargin
				)
		  } else -1
		}).getOrElse(show(s"filter failed"))
	}
	
	private def display(z: DblVector, x: DblVector): Unit =   {
		import org.scalaml.plots.{LinePlot, LightPlotTheme, Legend}
		
		val labels = Legend( 
			name, "Multi-variate linear regression", s"Raw vs. filtered", "y"
		)
		val data = (z, "Delta price") :: (x, "Filtered") :: List[(DblVector, String)]()
		LinePlot.display(data, labels, new LightPlotTheme)
	}
}


// ----------------------------  EOF ----------------------------------
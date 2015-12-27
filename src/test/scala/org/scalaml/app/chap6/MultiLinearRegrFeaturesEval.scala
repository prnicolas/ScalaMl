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
import org.scalaml.stats.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.Types.emptyString
import org.scalaml.workflow.data.{DataSink, DataSource}
import org.scalaml.supervised.regression.linear.MultiLinearRegression
import org.scalaml.filtering.movaverage.SimpleMovingAverage
import org.scalaml.util.{FormatUtils, DisplayUtils,  LoggingUtils}
import org.scalaml.app.Eval
import LoggingUtils._, XTSeries._, YahooFinancials._, MultiLinearRegression._, FormatUtils._, ScalaMl._



		/**
		 * '''Purpose:''' Singleton to test the multi-variate least squares regression. 
		 * This test evaluates the features selection capability of the multi-variate least 
		 * squares regression.
		 * 
		 * The example consists of creating, and evaluating different models (portfolio of exchanges
		 * traded funds) using the multi-nomial linear regression. The models are scored using the
		 * sum of squares errors and mean squared errors.
		 * 
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see org.scalaml.supervised.regression.linear
		 * @see Scala for Machine Learning Chapter 6 ''Regression and regularization '' / 
		 * Ordinary least  squares regression 
		 */
object MultiLinearRegrFeaturesEval extends Eval {
	import scala.collection.mutable.ListBuffer	

	final val SMOOTHING_PERIOD: Int = 16
	
	type Models = List[(Array[String], DblMatrix)]


		/**
		 * Name of the evaluation 
		 */
	val name: String = "MultiLinearRegrFeaturesEval"
 
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

		show(s"\n$header Ordinary least squares regression FEATURE SELECTION")
 
		
		val path = "resources/data/chap6/"
		val output = "output/chap6/CNY_output.csv"
		val symbols = Array[String]("CNY", "GLD", "SPY", "TLT")

			// Selects a simple moving average to eliminate the noise from the data
		val movAvg = SimpleMovingAverage[Double](SMOOTHING_PERIOD)
			
			// Filter out the noise from data by applying a partial function implementing
			// as simple moving average.
		def filter(pfnMovAve: PartialFunction[DblVector, Try[DblVector]]): Try[Array[DblVector]] = Try {
		  symbols.map(s => DataSource(s"$path$s.csv", true, true, 1))
								.map( _.get( YahooFinancials.adjClose) )
								.map( _.get.slice(20, 800))
								.map( pfnMovAve(_))
								.map(_.get)
		}

		
		(for {
				// Extract the simple moving average partial function
			pfnMovAve <- Try(movAvg |>)
			
				// Filter out the noise
			smoothed <- filter(pfnMovAve)
			
				// Generates the different models to evaluates
			models <- createModels(smoothed)
			
				// Compute the residual sum of square errors for each model
			rsses <- Try(getModelsRss(models, smoothed))
			
				// Compute the mean and total sum of square errors for each model
			(mses, tss) <- totalSquaresError(models, smoothed.head)
		}
		yield {			
			show(s"${rsses.mkString("\n")}\n${mses.mkString("\n")}\nResidual error= $tss")
		}).get
	}
	

	private def getModelsRss(
			models: Models, 
			y: Array[DblVector]): List[String] = {
		models.map{ case (labels, m) => s"${getRss(m.toVector, y.head, labels)}" }
	}

	
	private def totalSquaresError(
			models: Models, 
	    y: DblVector): Try[(List[String], Double)] = Try {
	  
		val errors = models.map{ case (labels, m) => rssSum(m, y)._1 }
		val mses = models.zip(errors).map{case(f, e) => s"MSE for ${f._1.mkString(" ")} = $e" }
 
		(mses, Math.sqrt(errors.sum)/models.size)
	}
  	  
 	private def getRss(xt: Vector[DblArray], y: DblVector, featureLabels: Array[String]): String = {
		val regression = new MultiLinearRegression[Double](xt, y)

		val modelStr = regression.weights.get.zipWithIndex.map{ case( w, n) =>
			val weights_str = format(w, emptyString, SHORT)
			if(n == 0 )
				s"${featureLabels(n)} = $weights_str"
			else
				s"$weights_st}.${featureLabels(n)}"
		}.mkString(" + ")
		s"model: $modelStr\n RSS = ${regression.get.rss}"
	}
 	
 	
 	def createModels(input: Array[DblVector]): Try[Models] = Try {
 		val features = input.drop(1).map(_.toArray)
		  	  
			// Retrieve the input variables by removing the first 
			// time series (labeled dataset) and transpose the array

		List[(Array[String], DblMatrix)](
				(Array[String]("CNY", "SPY" , "GLD", "TLT"), features.transpose),
				(Array[String]("CNY", "GLD", "TLT"), features.drop(1).transpose),
				(Array[String]("CNY", "SPY", "GLD"), features.take(2).transpose),
				(Array[String]("CNY", "SPY", "TLT"), features.zipWithIndex.filter( _._2 != 1)
																		.map( _._1)
																		.transpose),
				(Array[String]("CNY", "GLD"), features.slice(1,2).transpose)
		)
 	}
 	

	private def rssSum(xt: DblMatrix, expected: DblVector): DblPair = {
		val regression = MultiLinearRegression[Double](xt, expected)
		val pfnRegr = regression |>

		(regression.rss.get, sse(expected.toArray, xt.map(pfnRegr(_).get)))
	}
}


// ----------------------------  EOF ----------------------------------
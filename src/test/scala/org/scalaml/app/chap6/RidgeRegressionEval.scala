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

import scala.language.postfixOps

import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import org.scalaml.stats.{Difference, XTSeries}
import org.scalaml.core.Types.{ScalaMl, emptyString}
import org.scalaml.core.Types.ScalaMl.{DblArray,DblVector}
import org.scalaml.supervised.regression.linear.RidgeRegression
import org.scalaml.supervised.regression.Regression._
import org.scalaml.util.{DisplayUtils, FormatUtils,  LoggingUtils}
import org.scalaml.app.Eval
import LoggingUtils._, RidgeRegression._, XTSeries.Transpose._, Difference._, FormatUtils._, XTSeries._

		/**
		 * '''Purpose:''' Singleton to evaluate the Ridge regression classifier on 
		 * technical analysis of an Exchange Traded Fund.
		 * 
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see org.scalaml.supervised.regression.linear
		 * @see: Scala for Machine Learning  Chapter 6 ''Regression and regularization'' / Ridge 
		 * regression
		 */
object RidgeRegressionEval extends Eval {
	import scala.util.Try
	import org.apache.log4j.Logger
	import ScalaMl._, YahooFinancials._
  
		/**
		 * Name of the evaluation 
		 */
	val name: String = "RidgeRegressionEval"
	
	private val path = "resources/data/chap6/CU.csv"
	private val dataInput = "output/chap6/CU_input.csv"
	final val LAMBDA: Double = 0.5
	
		 /**
		 * Execution of the scalatest for '''RidgeRegression''' class.
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
		import YahooFinancials._
		show(s"$header Evaluation of Ridge regression")
		
		
		val src = DataSource(path, true, true, 1)
				
		(for {
				// Load historical stock price
			price <- src.get(adjClose)
			
				// Load then compute the historical stock relative volatility within a trading session
			volatility <- src.get(volatility)
			
				// Load then compute the historical stock relative trading volume
			volume <- src.get(volume)
			
				// Extract the features value and labels through differentiation
			(features, expected) <- differentialData(volatility, volume, price, diffDouble)
			
				// Generate a regression model using L2 penalty
			regression <- RidgeRegression[Double](features, expected, LAMBDA)
		} 
		yield {
		  		
			// Use the model created through training
			if( regression.isModel ) {
				val weightsStr = regression.weights.get.view.zipWithIndex
										.map{case (w, n) => s"${n}-${format(w, ": ", SHORT)}"} 
				val trend = features.map( dot(_, regression.weights.get) ) 
	
				show( s"""Weights\n ${weightsStr.mkString(" ")}
						| \nDelta:\n${expected.mkString(",")}\nTrend: ${trend.mkString(",")}
						| ${format(regression.rss.get, "rss =", MEDIUM)}""".stripMargin
				)
	
			 	
						// Create two predictors to be evaluated
				val y1 = predict(0.2, expected, volatility, volume)
				val y2 = predict(5.0, expected, volatility, volume)
				display(expected, y1, y2, 0.2, 5.0)
					
				val output = (2 until 10 by 2).map( n => { 
					val lambda = n*0.1
					val y = predict(lambda, expected, volatility, volume)
					s"Lambda  $lambda\n${format(y, emptyString, SHORT)}"
				})
				show(output.mkString(".")) 
			}
			else
				error("RidgeRegression failed")
		}).get
	}

		/**
		 * Computes the residual sum fo squares.
		 */
	private def rss(
			lambda: Double, 
			expected: DblVector, 
			volatility: DblVector, 
			volume: DblVector): Option[Double] = {
		(new RidgeRegression[Double](zipToSeries(volatility, volume, 1), expected, lambda)).rss
	}
   
	private def predict(
			lambda: Double, 
			expected: DblVector, 
			volatility: DblVector, 
			volume: DblVector): DblVector = {
	  
		val observations = zipToSeries(volatility, volume, 1)
		val regression = new RidgeRegression[Double](observations, expected, lambda)
		val fnRegr = regression |>
		
		observations.map(x => if( fnRegr.isDefinedAt(x) )fnRegr(x).get else Double.NaN)
	}
	
	private def display(
			z: DblVector, 
			y1: DblVector, 
			y2: DblVector, 
			lambda1: Double, 
			lambda2: Double): Unit = {
		import org.scalaml.plots.{LinePlot, LightPlotTheme, Legend}
	  
		val labels = Legend( 
			name, "Ridge Regression", s"Impact L2 penalty value", "y"
		)
		val data = (z, "Delta price") :: 
							(y1, s"L2 lambda $lambda1") :: 
							(y2, s"L2 lambda $lambda2") :: List[(DblVector, String)]()

		LinePlot.display(data, labels, new LightPlotTheme)
	}
}


// ----------------------------  EOF ----------------------------------
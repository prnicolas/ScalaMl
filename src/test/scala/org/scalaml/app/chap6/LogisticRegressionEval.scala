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

import org.apache.log4j.Logger
import org.apache.commons.math3.fitting.leastsquares.LevenbergMarquardtOptimizer

import org.scalaml.stats.{Difference, XTSeries, Transpose}
import org.scalaml.core.Types.emptyString
import org.scalaml.core.Types.ScalaMl.{DblMatrix, DblArray, DblVector}
import org.scalaml.supervised.regression.logistic.LogisticRegression
import org.scalaml.libraries.commonsmath.LogisticRegressionOptimizer
import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.YahooFinancials
import org.scalaml.util.{FormatUtils, DisplayUtils,  LoggingUtils}
import LoggingUtils._, DisplayUtils._, Transpose._, Difference._, XTSeries._, FormatUtils._
import org.scalaml.app.Eval


		/**
		 * '''Purpose:''' Singleton to evaluate the logistic regression classifier on 
		 * technical analysis of an Exchange Traded Fund.
		 * 
		 * @author Patrick Nicolas
		 * @version 0.98.3
		 * @see org.scalaml.supervised.regression.logistic
		 * @see Scala for Machine Learning  Chapter 6 ''Regression and regularization'' / Logistic
		 * regression
		 */
object LogisticRegressionEval extends Eval {
	import scala.util.{Random, Try}

		/**
		 * Name of the evaluation 
		 */
	val name: String = "LogisticRegressionEval"
	   
	private val path = "resources/data/chap6/CU.csv"   
	private val maxIters = 250
	private val maxEvals = 4500
	private val eps = 1e-7
	
		/**
		 * Execution of the scalatest for '''LogisticRegression''' class.
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
		import YahooFinancials._
	  
		show(s"$header Evaluation of Binomial Logistic regression")
		
		val src = DataSource(path, true, true, 1)
		
			// Select the non-linear optimizer for minimizing the loss function
		val optimizer = new LevenbergMarquardtOptimizer
		
		(for {
				// Load historical stock price
			price <- src.get(adjClose)
			
				// Load historical stock relative volatility
			volatility <- src.get(volatility)
			
				// Load historical stock relative volume
			volume <- src.get(volume)
			
				// Extract the features value and labels through differentiation
			(features, expected) <- differentialData(volatility, volume, price, diffInt)
			
				// Set the configuration for the mimization of the loss function
			lsOpt <- LogisticRegressionOptimizer(maxIters, maxEvals, eps, optimizer)
			
				// Create a logistic regression model through training using features as input
				// data and expected as labels
			regr <- LogisticRegression[Double](features, expected, lsOpt)
			
				// Extract the partial function logistic predictor
			pfnRegr <- Try (regr |>)
		}
		yield {	 
				// Generates prediction  values..
			val predicted = features.map(x => if( pfnRegr.isDefinedAt(x) ) pfnRegr(x).get else -1.0)
				
				// If succeeds, compute the accuracy as the percentage of properly classified observations
			if( !predicted.exists( _ == -1.0)) {
				val delta = predicted.view.zip(expected.view).map{ case(p, e) => if(p == e) 1 else 0 }
				val accuracy = delta.sum.toDouble/expected.size
				
				show(s"${LogisticRegressionEval.toString(regr)}\nAccuracy: $accuracy")
			}
			else 
			  error("Could not apply the logistic regression predictive partial function")
		}).get
	}
  

	private def toString(regression: LogisticRegression[Double]): String = 
		s"""Regression model ---:\n 
		| ${format(regression.rss.get, "Rss",SHORT)}\nWeights:
		| ${regression.weights
									.map(format(_, "", MEDIUM))
									.mkString(" ")}""".stripMargin
}

// --------------------------------  EOF ---------------------------------------
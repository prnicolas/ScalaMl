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
package org.scalaml.app.chap6

import org.apache.log4j.Logger
import org.apache.commons.math3.fitting.leastsquares.LevenbergMarquardtOptimizer

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl.{DblMatrix, DblVector}
import org.scalaml.supervised.regression.logistic.{LogisticRegressionOptimizer, LogisticRegression}
import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.YahooFinancials
import org.scalaml.util.{FormatUtils, DisplayUtils}
import org.scalaml.app.Eval


		/**
		 * <p><b>Purpose:</b> Singleton to evaluate the logistic regression classifier on 
		 * technical analysis of an Exchange Traded Fund.</p>
		 * 
		 * @author Patrick Nicolas
		 * @note: Scala for Machine Learning  Chapter 6 Regression and regularization / Logistic 
		 * regression
		 */
object LogisticRegressionEval extends Eval {
	import scala.util.{Random, Try, Success, Failure}

		/**
		 * Name of the evaluation 
		 */
	val name: String = "LogisticRegressionEval"
	   
	private val path = "resources/data/chap6/CU.csv"   
	private val maxIters = 250
	private val maxEvals = 4500
	private val eps = 1e-7
	
		/**
		 * <p>Execution of the scalatest for <b>LogisticRegression</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Evaluation of Binomial Logistic regression", logger)
		
		Try {
			val src = DataSource(path, true, true, 1)
			val price = src |> YahooFinancials.adjClose
			val volatility = src |> YahooFinancials.volatility 
			val volume = src |> YahooFinancials.volume
			val prices = price.toArray
				  
			val deltaPrice = prices.drop(1).zip(prices.dropRight(1))
									.map(z => if( z._1 > z._2) 1 else 0)
			val data =  volatility.zip(volume).map(z => Array[Double](z._1, z._2))

			val features: DblMatrix = data.toArray.dropRight(1)
			val lsOptimizer = LogisticRegressionOptimizer(maxIters, maxEvals, eps, 
					new LevenbergMarquardtOptimizer)
			val regression = LogisticRegression[Double](XTSeries[DblVector](features), deltaPrice, 
					lsOptimizer)
					
			if( regression.isModel ) {
				DisplayUtils.show(s"$name model: ${toString(regression)}",  logger)	
				val predicted = features.map(ft => (regression |> ft))
				
				val comparison = predicted.zip(deltaPrice).map(pd => if(pd._1 == pd._2) 1 else 0)
				val accuracy = comparison.sum.toDouble/deltaPrice.size
				DisplayUtils.show(s"$name Accuracy: $accuracy", logger)
			}
			else
				DisplayUtils.error(s"${name}.run Could not create a model", logger)
		} 
		match {
			case Success(n) =>n
			case Failure(e) => failureHandler(e)
		}
	}
  

	private def toString(regression: LogisticRegression[Double]): String = 
		if( regression.isModel ) {
			s"Regression model RSS: ${FormatUtils.format(regression.rss.get,"",FormatUtils.ShortFormat)}" +
			"\nWeights: " +
			regression.weights.get
										.map(FormatUtils.format(_, "", FormatUtils.MediumFormat))
										.mkString(" ")
		}
		else 
			"No regression model"
}


// --------------------------------  EOF ---------------------------------------
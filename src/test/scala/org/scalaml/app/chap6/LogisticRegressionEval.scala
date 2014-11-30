/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.app.chap6

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.Types.ScalaMl
import org.scalaml.supervised.regression.logistic._
import scala.util.{Random, Try, Success, Failure}
import org.apache.commons.math3.fitting.leastsquares.LevenbergMarquardtOptimizer
import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.YahooFinancials
import org.scalaml.util.Display
import org.apache.log4j.Logger
import org.scalaml.app.Eval


object LogisticRegressionEval extends Eval {
	val name: String = "LogisticRegressionEval"
	val maxExecutionTime: Int = 50
	
	private val logger = Logger.getLogger(name)
	   
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
		Display.show(s"\n\n *****  test#${Eval.testCount} $name Evaluation of Binomial Logistic regression", logger)
		
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
			val lsOptimizer = LogisticRegressionOptimizer(maxIters, maxEvals, eps, new LevenbergMarquardtOptimizer)
			val regression = LogisticRegression[Double](XTSeries[DblVector](features), deltaPrice, lsOptimizer)

			
			Display.show(s"$name ${toString(regression)}",  logger)		
			val predicted = features.map(ft => (regression |> ft))
	//		Display.show(s"$name Results of logistic regression prediction versus actual", logger)
			
			val comparison = predicted.zip(deltaPrice).map(pd => if(pd._1 == pd._2) 1 else 0)
			val accuracy = comparison.sum.toDouble/deltaPrice.size
			Display.show(s"$name Accuracy: $accuracy", logger)
		} 
		match {
			case Success(n) =>n
			case Failure(e) => Display.error(s"${name}.run", logger, e)
		}
	}
  

	private def toString(regression: LogisticRegression[Double]): String = {
		val buf = new StringBuilder(s"Regression model RSS = ${ScalaMl.toString(regression.rss.get, "", true)}\nWeights: ")
		regression.weights.get.foreach(w => buf.append(s"${ScalaMl.toString(w, "", false)}"))
		buf.toString
	}
}


// --------------------------------  EOF ---------------------------------------
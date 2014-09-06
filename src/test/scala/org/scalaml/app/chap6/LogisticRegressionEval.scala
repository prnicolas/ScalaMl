/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.app.chap6

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.supervised.regression.logistic._
import scala.util.Random
import org.apache.commons.math3.fitting.leastsquares.LevenbergMarquardtOptimizer
import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.YahooFinancials
import org.scalaml.util.Display
import org.apache.log4j.Logger


object LogisticRegressionEval {
   private val logger = Logger.getLogger("LogisticRegressionEval")
	   
   final val path = "resources/data/chap6/CU.csv"   
   final val maxIters = 250
   final val maxEvals = 4500
   final val eps = 1e-7

   def run: Unit = {
  	 Display.show("Evaluation of Binomial Logistic regression", logger)
		
  	 val src = DataSource(path, true, true, 1)
	 val price = src |> YahooFinancials.adjClose
	 val volatility = src |> YahooFinancials.volatility 
	 val volume = src |> YahooFinancials.volume
		
	 if( price != None && volatility != None && volume != None) {
	 	val prices = price.get.toArray
				  
		val deltaPrice = prices.drop(1).zip(prices.take(prices.size -1)).map(z => if( z._1 > z._2) 1 else 0)
		val data =  volatility.get.zip(volume.get).map(z => Array[Double](z._1, z._2))


		val features = data.take(data.size-1)
		val lsOptimizer = LogisticRegressionOptimizer(maxIters, maxEvals, eps, new LevenbergMarquardtOptimizer)
		val regression = LogisticRegression[Double](XTSeries[DblVector](features), deltaPrice, lsOptimizer)
		    
		Display.show(toString(regression), logger)		
	    val predicted = features.map(x => (regression |> x).get)
		Display.show(predicted, logger)
      }
   }
   

   private def toString(regression: LogisticRegression[Double]): String = {
  	  val buf = new StringBuilder("Regression model: RMS").append(regression.rss.get).append(" weights:")
  	  regression.weights.get.foreach(w => buf.append(" ").append(w))
  	  buf.toString
   }
}


// --------------------------------  EOF ---------------------------------------
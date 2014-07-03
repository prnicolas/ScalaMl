/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap7

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.supervised.regression.logistic._
import scala.util.Random
import org.apache.commons.math3.fitting.leastsquares.LevenbergMarquardtOptimizer
import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.PriceVolume


object LogisticRegressionEval {
   final val path = "resources/data/chap7/CU.csv"
   final val dataInput = "output/chap7/CU_input.csv"

  def run(args: Array[String] = null): Unit = {
		val src = DataSource(path, true, true, 1)
		val price = src |> PriceVolume.adjClose
		val volatility = src |> PriceVolume.volatility 
		val volume = src |> PriceVolume.volume
		
		if( price != None && volatility != None && volume != None) {
			val prices = price.get.toArray
				  
			val priceChange = prices.drop(1).zip(prices.take(prices.size -1)).map( z => if( z._1 > z._2) 1 else 0)
		    val data =  volatility.get.zip(volume.get).map(z => Array[Double](z._1, z._2))

		    val maxIters = 80
		    val maxEvals = 100
		    val features = data.take(data.size-1)
		    val lsOptimizer = LogisticRegressionOptimizer(maxIters, maxEvals, 1e-4, new LevenbergMarquardtOptimizer)
		    val logIt = new LogisticRegression[Double](XTSeries[DblVector](features) ,priceChange, lsOptimizer)
		    println("RMS: " +  logIt.rms.get )
		    logIt.weights.get.foreach( println )
       }
   }
}

// --------------------------------  EOF ---------------------------------------
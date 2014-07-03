/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap6

import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.PriceVolume
import PriceVolume._
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.supervised.regression.linear.RidgeRegression


object RidgeRegressionEval {
   final val path = "resources/data/chap7/CU.csv"
   final val dataInput = "output/chap7/CU_input.csv"
  	 
	def run(args: Array[String] = null): Unit = {
  	   	println("Evaluation of Ridge regression")
  	   	 
		val src = DataSource(path, true, true, 1)
		val price = src |> PriceVolume.adjClose
		val volatility = src |> PriceVolume.volatility 
		val volume = src |> PriceVolume.volume
		
		if( price != None && volatility != None && volume != None) {
			val prices = price.get.toArray
		    val deltaPrice = XTSeries[Double](prices.drop(1).zip(prices.take(prices.size -1)).map( z => z._1 - z._2))
		
		    DataSink[Double](dataInput) |> deltaPrice :: volatility.get :: volume.get :: List[XTSeries[Double]]()
		    val data =  volatility.get.zip(volume.get).map(z => Array[Double](z._1, z._2))
		
		    val features = XTSeries[DblVector](data.take(data.size-1))
		    val regression = new RidgeRegression[Double](features, 
				                                         deltaPrice, 0.5)
	        regression.weights match {
			   case Some(w) => w.zipWithIndex.foreach( wi => println(wi._1 +": " + wi._2))
			   case None => println("The multivariate regression could not be trained")
		    }
	    }
   }
}


// ----------------------------  EOF ----------------------------------
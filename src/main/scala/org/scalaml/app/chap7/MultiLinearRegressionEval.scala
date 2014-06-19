/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap7

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.PriceVolume
import PriceVolume._
import org.scalaml.core.XTSeries
import org.scalaml.workflow.data.DataSink
import org.scalaml.supervised.regression.linear.MultiLinearRegression
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.filtering.SimpleMovingAverage


object MultiLinearRegressionEval {

  	 
   def run(args: Array[String] = null) {
  	  if( args != null && args.size > 0 && args(0).equals("trend")) 
  	  	trend
  	  else
        filter  	  	
   }	 
   
   private[this] def trend: Unit = {
  	 
  	  val path = "resources/data/chap7/"
  	  val output = "output/chap7/CNY_input.csv"
  	  	
  	  val symbols = Array[String]("CNY", "GLD", "SPY", "TLT")
  	  
  	  val movAvg = SimpleMovingAverage[Double](8)
  	  val variables = symbols.map(s => DataSource(path + s+".csv", true, true, 1))
  	                         .map( _ |> PriceVolume.adjClose )
  	                         .map(x => movAvg |> XTSeries[Double](x.get))
  	    
  	  DataSink[Double](output) |>  variables.foldLeft(List[XTSeries[Double]]())((sk, v) => v.get :: sk)
   }
  	 
   private[this] def filter: Unit = {
  	 val path = "resources/data/chap7/CU.csv"
     val dataInput = "output/chap7/CU_input.csv"
  	 
	 val src = DataSource(path, true, true, 1)
	 val price = src |> PriceVolume.adjClose
	 val volatility = src |> PriceVolume.volatility 
	 val volume = src |> PriceVolume.volume
		
		if( price != None && volatility != None && volume != None) {
			val prices = price.get.arr
		    val deltaPrice = XTSeries[Double](prices.drop(1).zip(prices.take(prices.size -1)).map( z => z._1 - z._2))
		
		    DataSink[Double](dataInput) |> deltaPrice :: volatility.get :: volume.get :: List[XTSeries[Double]]()
		    val data =  volatility.get.arr.zip(volume.get.arr).map(z => Array[Double](z._1, z._2))
		
		    val regression = MultiLinearRegression[Double](XTSeries[DblVector](data.take(data.size-1)), 
				                                              deltaPrice)
	        regression.weights match {
			   case Some(w) => w.zipWithIndex.foreach( wi => println(wi._1 +": " + wi._2))
			   case None => println("The multivariate regression could not be trained")
		    }
	    }
   }
}


// ----------------------------  EOF ----------------------------------
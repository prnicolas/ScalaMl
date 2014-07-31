/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap6

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.YahooFinancials
import YahooFinancials._
import org.scalaml.core.XTSeries
import org.scalaml.workflow.data.DataSink
import org.scalaml.supervised.regression.linear.MultiLinearRegression
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.filtering.SimpleMovingAverage


object MultiLinearRegressionEval {

   def run(args: Array[String] = null) {
  	  if( args != null && args.size > 0 && args(0).equals("trend")) 
  	  	inference
  	  else
        filter  	  	
   }	 
   
   private[this] def inference: Unit = {
  	  println("Evaluation of ordinary least squares regression inference")
  	  
  	  val path = "resources/data/chap7/"
  	  val output = "output/chap7/CNY_output.csv"
  	  val symbols = Array[String]("CNY", "GLD", "SPY", "TLT")
  	  val movAvg = SimpleMovingAverage[Double](16)

  	  val input = symbols.map(s => DataSource(path + s +".csv", true, true, 1))
  	                     .map( _ |> YahooFinancials.adjClose )
  	                     .map(x => (movAvg |> XTSeries[Double](x.get.slice(20, 800))).get)
  	    
  	  DataSink[Double](output) |>  input.foldLeft(List[XTSeries[Double]]())((sk, v) => v :: sk)
  	  
  	  		// Retrieve the input variables by removing the first 
  	  		// time series (labeled dataset) and transpose the array
  	  val allVariables = input.drop(1)
  	  var variables = allVariables.map( _.toArray).transpose
  	  println("CNY = f(SPY, GLD, TLT)" )
  	  rss(XTSeries[DblVector](variables), input(0))
  	  
  	  println("CNY = f(GLD, TLT)" )
  	  variables = allVariables.drop(1).map( _.toArray).transpose
  	  rss(XTSeries[DblVector](variables), input(0))
  	  
  	  println("CNY = f(SPY, GLD)" )
  	  variables = allVariables.take(2).map( _.toArray).transpose
  	  rss(XTSeries[DblVector](variables), input(0))
  	  
  	  println("CNY = f(SPY, TLT)" )
  	  variables = allVariables.zipWithIndex.filter( _._2 != 1).map( _._1.toArray).transpose
  	  rss(XTSeries[DblVector](variables), input(0))
  	  
  	  println("CNY = f(GLD)" )
  	  variables = allVariables.slice(1,2).map( _.toArray).transpose
  	  rss(XTSeries[DblVector](variables), input(0))

   }
  	  
   private def rss(xt: XTSeries[DblVector], y: XTSeries[Double]): Unit = {
  	  val regression = MultiLinearRegression[Double](xt, y)
  	  val buf = new StringBuilder
  	  regression.weights
  	            .get
  	            .zipWithIndex
  	            .foreach(w => {
  	            	 if( w._2 == 0) buf.append(w._1)
  	            	 else buf.append(" + ").append(w._1).append(".X").append(w._2)
  	            })
  	  println(buf.toString)
  	  println( "RSS: " + regression.rss.get )
  }
  	 
   private[this] def filter: Unit = {
  	 println("Evaluation of ordinary least squares regression filtering")
  	   	 
  	 val path = "resources/data/chap7/CU.csv"
     val dataInput = "output/chap7/CU_input.csv"
  	 
	 val src = DataSource(path, true, true, 1)
	 val price = src |> YahooFinancials.adjClose
	 val volatility = src |> YahooFinancials.volatility 
	 val volume = src |> YahooFinancials.volume
		
	 if( price != None && volatility != None && volume != None) {
		val prices = price.get.toArray
	    val deltaPrice = XTSeries[Double](prices.drop(1).zip(prices.take(prices.size -1)).map( z => z._1 - z._2))
		
		DataSink[Double](dataInput) |> deltaPrice :: volatility.get :: volume.get :: List[XTSeries[Double]]()
		val data =  volatility.get.zip(volume.get).map(z => Array[Double](z._1, z._2))
		    
		val features = XTSeries[DblVector](data.take(data.size-1))
		val regression = MultiLinearRegression[Double](features, deltaPrice)
	    regression.weights match {
	       case Some(w) => w.zipWithIndex.foreach( wi => println(wi._1 +": " + wi._2))
		   case None => println("The multivariate regression could not be trained")
		}
	  }
   }
}


// ----------------------------  EOF ----------------------------------
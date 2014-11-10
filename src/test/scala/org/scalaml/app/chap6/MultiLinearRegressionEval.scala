/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95c
 */
package org.scalaml.app.chap6

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.YahooFinancials
import YahooFinancials._
import org.scalaml.core.XTSeries
import org.scalaml.workflow.data.DataSink
import org.scalaml.supervised.regression.linear.MultiLinearRegression
import org.scalaml.core.types.ScalaMl._
import org.scalaml.filtering.SimpleMovingAverage
import org.scalaml.util.Display
import org.apache.log4j.Logger
import scala.collection.mutable.ListBuffer
import scala.util.{Try, Success, Failure}
import org.scalaml.app.Eval


			/**
			 * <p>Singleton to test the multi-variate least squares regression. The evaluation
			 * is composed of two tests<br>
			 * Trend analysis and filter: MultiLinearRegressionEval.filter<br>
			 * Features selection: MultiLinearRegressionEval.featuresSelection
			 */
object MultiLinearRegressionEval extends Eval {
   val name: String = "MultiLinearRegressionEval"
   private val logger = Logger.getLogger(name)
   
   def run(args: Array[String] = null): Int = {
  	  if( args != null && args.size > 0 && args(0).equals("trend")) 
  	  	featuresSelection
  	  else
        filter  	  	
   }	 
   
   private def featuresSelection: Int = {
  	  Display.show(s"$name evaluation of ordinary least squares regression inference", logger)
  	  
  	  val path = "resources/data/chap6/"
  	  val output = "output/chap6/CNY_output.csv"
  	  val symbols = Array[String]("CNY", "GLD", "SPY", "TLT")
  	  val smoothingPeriod = 16
  	  val movAvg = SimpleMovingAverage[Double](smoothingPeriod)
      Try {
	  	  val input = symbols.map(s => DataSource(path + s +".csv", true, true, 1))
	  	                     .map( _ |> YahooFinancials.adjClose )
	  	                     .map(x => movAvg |> XTSeries[Double](x.slice(20, 800)))
	  	    
	  	  DataSink[Double](output) |>  input.foldLeft(List[XTSeries[Double]]())((sk, v) => v :: sk)
	  	  
	  	  		// Retrieve the input variables by removing the first 
	  	  		// time series (labeled dataset) and transpose the array
	  	  val features = input.drop(1)
	  	  val featuresList = List[(String, DblMatrix)](
	  	  		("CNY = f(SPY, GLD, TLT)\n", features.map( _.toArray).transpose),
	  	  		("CNY = f(GLD, TLT)\n", features.drop(1).map( _.toArray).transpose),
	  	  		("CNY = f(SPY, GLD)\n", features.take(2).map( _.toArray).transpose),
	  	  		("CNY = f(SPY, TLT)\n", features.zipWithIndex.filter( _._2 != 1).map( _._1.toArray).transpose),
	  	  		("CNY = f(GLD)\n", features.slice(1,2).map( _.toArray).transpose)
	  	  )
	  	  		
	  	  featuresList.foreach(x =>  Display.show(s"${x._1 + getRss(XTSeries[DblVector](x._2), input(0))}", logger ))  	  
	  	  
	  	  var xsRss = new ListBuffer[Double]()
	  	  val tss = featuresList.foldLeft(0.0)((s, x) => {
	  	  	 val _tss = rssSum(XTSeries[DblVector](x._2), input(0))
	  	  	 xsRss.append(_tss._1)
	  	  	 s + _tss._2
	  	  })/xsRss.size
	  	  
	  	  val r2 = xsRss.map( 1.0 - _/tss)
	  	  Display.show(r2, logger)
  	  } match {
  	  	case Success(n) => n
  	  	case Failure(e) => Display.error(s"$name MultiLinearRegressionEval.inference", logger, e)
  	  }
   }
  	  
   private def getRss(xt: XTSeries[DblVector], y: DblVector): String = {
  	  val regression = MultiLinearRegression[Double](xt, y)
  	  val buf = new StringBuilder
  	  regression.weights
  	            .get
  	            .zipWithIndex
  	            .foreach(w => {
  	               if( w._2 == 0) buf.append(w._1)
  	               else buf.append(s" + ${w._1}.x${w._2}")
  	            })
  	  buf.append(s"$name RSS: ${regression.rss.get}").toString
  }
   
  
   
  private def rssSum(xt: XTSeries[DblVector], y: DblVector): XY = {
  	  val regression = MultiLinearRegression[Double](xt, y)
  	  
  	  (regression.rss.get, xt.toArray.foldLeft(0.0)((s, x) => s + (regression |> x)))
  }
  
  	 
   private def filter: Int = {
  	 Display.show(s"$name Evaluation of ordinary least squares regression filtering", logger)
  	   	 
  	 val path = "resources/data/chap6/CU.csv"
     val output = "output/chap6/CU_output.csv"
  	 
     Try {
   	    val src = DataSource(path, true, true, 1)
	    val prices = (src |> YahooFinancials.adjClose).toArray 
	    val volatility = src |> YahooFinancials.volatility 
	    val volume = src |> YahooFinancials.volume
	
		val deltaPrice = prices.drop(1)
		                       .zip(prices.take(prices.size -1))
		                       .map(z => z._1 - z._2)
	    
		DataSink[Double](output) |>  XTSeries[Double](deltaPrice) ::
                                    volatility :: 
                                    volume :: 
                                    List[XTSeries[Double]]()
                                   
		val data =  volatility.zip(volume).map(z => Array[Double](z._1, z._2))
		    
		val features = XTSeries[DblVector](data.take(data.size-1))
		val regression = MultiLinearRegression[Double](features, deltaPrice)
	    regression.weights match {
	       case Some(w) => {
	      	   val buf = new StringBuilder("MultiLinearRegressionEval.filter: ")
	      	   w.zipWithIndex.foreach( wi => buf.append(s"${wi._1}: ${wi._2}\n"))
	      	   Display.show(buf.toString, logger)
	       }
		   case None => Display.error(s"$name  multivariate regression could not be trained", logger)
		}
		Display.show(s"$name MultiLinearRegressionEval.filter", logger)
	  }
  	 
  	  match {
  	  	case Success(n) => n
  	  	case Failure(e) => Display.error(s"$name filter", logger, e)
  	  }
   }
}


// ----------------------------  EOF ----------------------------------
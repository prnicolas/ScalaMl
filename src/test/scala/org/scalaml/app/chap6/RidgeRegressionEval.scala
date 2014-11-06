/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95
 */
package org.scalaml.app.chap6

import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import YahooFinancials._
import org.scalaml.core.XTSeries
import org.scalaml.core.types.ScalaMl._
import org.scalaml.supervised.regression.linear.RidgeRegression
import org.scalaml.util.Display
import org.apache.log4j.Logger
import scala.util.{Try, Success, Failure}

object RidgeRegressionEval {
   final val path = "resources/data/chap6/CU.csv"
   final val dataInput = "output/chap6/CU_input.csv"
    
   private val logger = Logger.getLogger("RidgeRegressionEval")	 
   
	def run: Int = {
  	   	Display.show("Evaluation of Ridge regression", logger)
  	   	 
  	   	Try {
		   val src = DataSource(path, true, true, 1)
		   val price = src |> YahooFinancials.adjClose
		   val volatility = src |> YahooFinancials.volatility 
		   val volume = src |> YahooFinancials.volume
		
		   val deltaPrice = XTSeries[Double](price.drop(1).zip(price.take(price.size -1)).map( z => z._1 - z._2))
		
		    DataSink[Double](dataInput) |> deltaPrice :: 
		                                   volatility :: 
		                                   volume :: List[XTSeries[Double]]()
		    val data =  volatility.zip(volume).map(z => Array[Double](z._1, z._2))
		
		    val features = XTSeries[DblVector](data.take(data.size-1))
		    val regression = new RidgeRegression[Double](features, 
				                                         deltaPrice, 0.5)
	        regression.weights match {
			   case Some(w) => w.zipWithIndex.foreach( wi => Display.show(s"{wi._1}: ${wi._2}", logger))
			   case None => Display.error("The multivariate regression could not be trained", logger)
		    }
		    
		    regression.rss match {
		    	case Some(rss) => Display.show(rss, logger)
		    	case None => Display.error("The multivariate regression could not be trained", logger)
		    }
		    
		    Display.show((1 until 10 by 2), logger)
		} match {
			case Success(n) => n
			case Failure(e) => Display.error("RidgeRegressionEval.run", logger, e)
		}
  	   	
   }
   
   private def rss(lambda: Double, deltaPrice: DblVector, volatility: DblVector, volume: DblVector): Double = {
  	  val data =  volatility.zip(volume).map(z => Array[Double](z._1, z._2))
		
	  val features = XTSeries[DblVector](data.take(data.size-1))
	  val regression = new RidgeRegression[Double](features, 
				                                   deltaPrice, lambda)
      regression.rss.get
   }
   
   private def predict(lambda: Double, deltaPrice: DblVector, volatility: DblVector, volume: DblVector): DblVector = {
  	  val data =  volatility.zip(volume).map(z => Array[Double](z._1, z._2))
		
	  val features = XTSeries[DblVector](data.take(data.size-1))
	  val regression = new RidgeRegression[Double](features, 
				                                   deltaPrice, lambda)
	  features.foldLeft(0.0)((s, x) => s + (regression |> x))
   }
}


// ----------------------------  EOF ----------------------------------
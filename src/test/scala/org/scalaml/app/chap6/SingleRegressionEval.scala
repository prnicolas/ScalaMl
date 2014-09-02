/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.app.chap6

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.YahooFinancials
import org.scalaml.supervised.regression.linear.SingleLinearRegression
import YahooFinancials._
import org.scalaml.core.Types.ScalaMl._
import org.apache.log4j.Logger
import org.scalaml.util.Display
import org.scalaml.core.XTSeries


import XTSeries._
object SingleLinearRegressionEval {
			
	final val path = "resources/data/chap6/CU.csv"
	private val logger = Logger.getLogger("SingleLinearRegressionEval")
		
	def run: Unit =  {
		println("Evaluation of single variate linear regression")
		
	    DataSource(path, false, true, 1) |> adjClose match {
	    	case Some(price) => {
	  // 		val xyt = XTSeries[Double](Range(1, 100).map(x => x*2 - 6).toArray)
	    		
	    		val xy = price.zipWithIndex.map(x => (x._2.toDouble, x._1.toDouble))
	    		val linRegr = SingleLinearRegression(xy)
	    
	    	    val slope = linRegr.slope
	    	    val intercept = linRegr.intercept
	    	    if( slope != None ) {
	    	       println("y = " + slope + ".x + " + intercept)
	    	       println(validate(xy.toArray, slope.get, intercept.get))
	    	    }
	    		/*
	    			case Some(w1) => println("slope: " + w1)  //Display.show("slope: " + w1, logger)
	    			case None => Display.error("SingleLinearRegressionEval.run: no model for single linear regression", logger)
	    		}
	    		linRegr.intercept match {
	    			case Some(w0) => println("intercept: " + w0) //Display.show("intercept: " + w0, logger)
	    			case None => Display.error("SingleLinearRegressionEval.run: no model for single linear regression", logger)
	    		}
	    		println("Volume")
	    		
	    		println(validate(xy, slope, ))
	    		
	    	    linRegr |> xy(4)._1 match {
	    	    	case Some(predicted) => Display.show("predicted: " + predicted, logger)
	    	    	case None => Display.error("SingleLinearRegressionEval.run: no model for single linear regression", logger)
	    	    }
	    	    * */
	    	    
	    	}
	    	case None => Display.error("Cannot extract price from " + path, logger)
	    }
	}
	
	private def validate(xyt: Array[(Double, Double)], slope: Double, intercept: Double): Double = {
	   val error = xyt.foldLeft(0.0)((err, xy) => {
		 val diff = xy._2 - slope*xy._1 - intercept
		 println( xy._2 + " - " + (slope*xy._1 + intercept))
	     err + diff*diff  
		})
		Math.sqrt(error/xyt.size)
	}
}


object SingleLinearRegressionEvalApp extends App {
	 SingleLinearRegressionEval.run
}


// ----------------------------  EOF ----------------------------------
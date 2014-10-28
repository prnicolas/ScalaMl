/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.94
 */
package org.scalaml.app.chap6

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.YahooFinancials
import org.scalaml.supervised.regression.linear.SingleLinearRegression
import YahooFinancials._
import org.scalaml.core.types.ScalaMl._
import org.apache.log4j.Logger
import org.scalaml.util.Display
import org.scalaml.core.XTSeries
import scala.util.{Try, Success, Failure}

import XTSeries._
object SingleLinearRegressionEval {
			
	final val path = "resources/data/chap6/CU.csv"
	private val logger = Logger.getLogger("SingleLinearRegressionEval")
		
	def run: Int =  {
		Display.show("Evaluation of single variate linear regression", logger)
		
		Try {
		    val price = DataSource(path, false, true, 1) |> adjClose
		    val xy = price.zipWithIndex.map(x => (x._2.toDouble, x._1.toDouble))
		    val linRegr = SingleLinearRegression(xy)
		    
		    val slope = linRegr.slope
		    val intercept = linRegr.intercept
		    if( slope != None ) {
		    	Display.show(s"y = $slope.x + $intercept", logger)
		    	Display.show(s"validation: ${validate(xy.toArray, slope.get, intercept.get)}", logger)
		    }
		    else
		    	Display.error("SingleLinearRegressionEval.run failed to get slope", logger)
		    	  
		  }match {
		  	case Success(n) => n
		  	case Failure(e) => Display.error("SingleLinearRegressionEval.run", logger, e)
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
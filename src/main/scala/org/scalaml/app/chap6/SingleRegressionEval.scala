/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap6

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.PriceVolume
import org.scalaml.supervised.regression.linear.SingleLinearRegression
import PriceVolume._
import org.scalaml.core.Types.ScalaMl._




object SingleLinearRegressionEval {
			
	final val path = "resources/data/chap7/CU.csv"
		
	def run(args: Array[String] = null): Unit =  {
		println("Evaluation of single variate linear regression")
		  	 
		val src = DataSource(path, false, true, 1)
		
	    src |> adjClose match {
	    	case Some(price) => {
	    		val linRegr = SingleLinearRegression(price)
	    		linRegr.wr match {
	    			case Some(w) => println("y = " + w._1 + ".x + " + w._2)
	    			case None => println("Linear regression could not be trained")
	    		}
	    		println("Volume")
	    	    linRegr |> price.size match {
	    	    	case Some(predicted) => println("predicted: " + predicted)
	    	    	case None => println("Linear regression prediction failed")
	    	    }
	    	}
	    	case None => println("Cannot extract price from " + path)
	    }
	}
}



// ----------------------------  EOF ----------------------------------
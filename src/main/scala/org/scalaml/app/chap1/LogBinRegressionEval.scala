/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap1

import scala.io.Source
import java.awt.Color
import org.scalaml.plots._
import org.scalaml.stats.Stats
import org.scalaml.trading.{Signal, PriceVolume}
import org.scalaml.core.Types
import Signal._
import Types.ScalaMl._
import org.scalaml.supervised.regression.LogBinRegression



		/**
		 * <p>Test driver for the Logistic two-class Regression presented in Chapter 1.</p>
		 * @author Patrick Nicolas
		 * @date December 22, 2013
		 * @project Scala for Machine Learning
		 * @project Scala for Machine Learning.
		 */
object LogBinRegressionEval  {
	import PriceVolume._
	
	    
    final val nIters = 300
    final val eps = 0.02
    final val eta = 0.000002

    		/*
    		 * Driver code that load, visualize and train labeled data.
    		 * Classifier is invoked on the model once training is completed.
    		 */
    def run(args: Array[String]) {
	    load("resources/data/chap1/CSCO.csv") match {
	      case Some(volatilityVolume) => {
	      	
	        display(volatilityVolume)
	    	
	        val labels = volatilityVolume.zip(volatilityVolume.map(x => if( x._1 > 0.32 && x._2 > 0.35) 1.0 else 0.0 ))
		    val logit = new LogBinRegression(labels, nIters, eta, eps)
	    	  
	    	load("resources/data/chap1/CSCO2.csv") match {
	    	   case Some(test) =>{
	    	  	 logit.classify(test(0)) match {
		            case Some(topCategory) => println(topCategory)
		            case None => println("Failed to classify")
		         }
	    	  	 logit.classify(test(1)) match {
		            case Some(topCategory) => println(topCategory)
		            case None => println("Failed to classify")
		         }
	    	   }	
	    	   case None => println("Could not load stock information for CSCO2")
	    	 }
	      }
	      case None => println("Could not load stock information for CSCO")
	    }
    }
	
			/**
		 * Method to load and normalize the volume and volatility of a stock.
		 */
    private def load(fileName: String): Option[XYTSeries] = {
    	import java.io.{FileNotFoundException, IOException}
    	
    	require(fileName != null, "Cannot load data from undefined fileName")
    	try {
			val src =  Source.fromFile(fileName)
			val fields = src.getLines.map( _.split(CSV_DELIM)).toArray
			val cols = fields.drop(1)
		    val volatility = Stats[Double]( cols.map( f => f(HIGH.id).toDouble - f(LOW.id).toDouble ) ).normalizeMean
			val volume =  (Stats[Double])(cols.map( _(VOLUME.id).toDouble) ).normalizeMean
			src.close
			Some(volatility.zip(volume))
    	}
    	catch {
    		case e: IllegalArgumentException => println("Incorrect argument: " + e.toString); None
    		case e: FileNotFoundException => println("File not found: " + e.toString); None
    		case e: IOException => println("IOException : " + e.toString); None
    		case e: Exception => println(e.toString); None
    	}
    }
    
    	/**
    	 * Method to display a time series
    	 */
    private def display(volatilityVolume: XYTSeries): Unit = {
       require(volatilityVolume != null && volatilityVolume.size > 0, "Cannot display an undefined time series")
       
       val plotter = new ScatterPlot(("CSCO 2012-2013 Stock", "Session Volatility", "Session Volume"), new BlackPlotTheme)
       plotter.display(volatilityVolume.filter(x => (x._2 < 0.4 && x._2 > -0.48) && x._1 < 0.40), 250, 340)
	}

    
}


// --------------------  EOF --------------------------------------
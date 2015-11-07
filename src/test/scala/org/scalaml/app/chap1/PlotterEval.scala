/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99
 */
package org.scalaml.app.chap1

import scala.util.{Try, Success, Failure}
import scala.io.Source
	
import org.apache.log4j.Logger
	
import org.scalaml.core.Types.ScalaMl.{DblVector, DblArray}
import org.scalaml.stats.{Stats, MinMax}
import org.scalaml.plots._
import org.scalaml.util.{DisplayUtils,  LoggingUtils}
import LoggingUtils._
import org.scalaml.app.Eval

		/**
		 * '''Purpose'''Singleton to evaluate the different categories of plot
		 * (line, scatter.)  used inthe Scala for Machine learning
		 * 
		 * @author Patrick Nicolas
		 * @since 0.97 November 17, 2013
		 * @version 0.98.1
		 * @see Scala for Machine Learning Chapter 1 "Getting Started" Let's kick the tires / Plotting
		 */
object PlotterEval extends Eval {
		/**
		 * Name of the evaluation 
		 */
	val name: String = "PlotterEval"

	private val CSV_DELIM = ","
	private val PRICE_COLUMN_INDEX = 6
	private val OPEN_COLUMN_INDEX = 1
	private val VOL_COLUMN_INDEX = 5
	private val HIGH_INDEX = 2
	private val LOW_INDEX = 3
	private val pathName = "resources/data/chap1/CSCO.csv"
    
 
		/**
		 * Execution of the '''scalatest''' for LinePlot and ScatterPlot classes. 
		 * This method is invoked by the actor-based test framework function, ScalaMlTest.evaluate.
		 * 
		 * Exceptions thrown during the execution of the tests are caught by the wrapper or handler
		 * test method in Eval trait defined as follows:
		 * {{{
		 *    def test(args: Array[String]) =
		 *      Try(run(args)) match {
		 *        case Success(n) => ...
		 *        case Failure(e) => ...
		 * }}}
		 * The tests can be executed through ''sbt run'' or individually by calling 
		 * ''TestName.test(args)'' (i.e. DKalmanEval.test(Array[String]("IBM") )
		 * @param args array of arguments used in the test
		 */
	override protected def run(args: Array[String]): Int = {
		show(s"$header Evaluation of interface to JFreeChart library")
		
		  	// Load data from the source.
		val src = Source.fromFile(pathName)
		val fields = src.getLines.map( _.split(CSV_DELIM))	   
			
		val cols = fields.drop(1)
		val volatilityVolume = cols.map(f => 
			(f(HIGH_INDEX).toDouble - f(LOW_INDEX).toDouble, 
			f(VOL_COLUMN_INDEX).toDouble)).toVector.unzip
		
			
		val volatility = MinMax[Double](volatilityVolume._1).get.normalize(0.0, 1.0) 
		val normVolume =  MinMax[Double](volatilityVolume._2).get.normalize(0.0, 1.0)
			
		show(s"Line plot for CSCO stock normalized volume")
		val labels1 = Legend(
			name, "Line plotting CSCO 2012-13 Stock volume", "Volume", "r"
		)
			
		LinePlot.display(normVolume, labels1, new LightPlotTheme)

		show(s"Line plot for CSCO stock volatility")
		val labels2 = Legend(
			name, "Line plotting CSCO 2012-13 Stock Volatility", "Volatility", "r"
		)	
		ScatterPlot.display(volatility.toArray, labels2, new BlackPlotTheme)
		    	
		show(s"Scatter plot CSCO stock volatility vs. volume")
		val labels3 = Legend(
				name, "Line plotting CSCO 2012-2013 volatility vs. volume", "Volatility vs. Volume", "r"
		)	
		ScatterPlot.display(volatility.zip(normVolume.view), labels3, new LightPlotTheme)
		src.close
		1
	}
}


// ---------------------------------------  EOF ----------------------------------------------
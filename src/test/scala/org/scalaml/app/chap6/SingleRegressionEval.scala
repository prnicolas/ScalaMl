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
package org.scalaml.app.chap6

import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger

import org.scalaml.core.Types.ScalaMl.{DblPair, DblVector}
import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.YahooFinancials
import org.scalaml.supervised.regression.linear.SingleLinearRegression
import org.scalaml.util.{FormatUtils, DisplayUtils,  LoggingUtils}
import org.scalaml.stats.XTSeries
import org.scalaml.app.Eval
import LoggingUtils._, YahooFinancials._, XTSeries._, FormatUtils._

		/**
		 * '''Purpose:''' Singleton to evaluate the single variate linear regression.
		 * 
		 * The test consists of creating a linear regression y = slope.x + intercept for a stock price
		 * 
		 * @author Patrick Nicolas
		 * @version 0.99
	   * @see: Scala for Machine Learning Chapter 6: ''Regression and regularization'' / One-variate 
	   * linear regression
		 */ 
object SingleLinearRegressionEval extends Eval {
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "SingleLinearRegressionEval"

	private val path = "resources/data/chap6/CU.csv"

		/**
		 * Execution of the scalatest for '''SingleLinearRegression''' class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate
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
	override protected def run(args: Array[String]): Int =  {
		show(s"$header Evaluation of single variate linear regression")
		

		(for {
				// Price is the expected values
			price <- DataSource(path, false, true, 1) get adjClose
			
				// Generates the trading days for X-axis and create
				// a single linear regression model
			days <- Try( Vector.tabulate(price.size)(_.toDouble))
			linRegr <- SingleLinearRegression[Double](days, price)
		} 
		yield {
			if(linRegr.isModel ) {
				val slope = linRegr.slope.get
				val intercept = linRegr.intercept.get
			
					// Display the raw and regressed prices on a line plot.
				val values  = predict(days, price, slope, intercept).unzip
				val entries = List[DblVector](values._1, values._2)
				
				display(entries.zip(List[String]("raw price", "Linear regression")))
				
					// Dump model parameters and tabulate data into standard out.
				val results = s"""$name regression ${format(slope, "y= ", SHORT)}.x +
									| ${format(intercept, " ", SHORT)}
									| \nError: ${mse(days, price, slope, intercept)}""".stripMargin
	
									
				show(s"$results\n${tabulate(days, price, slope, intercept)}")
			} else -1
		}).get
	}
	
	
	private def display(values: List[(DblVector, String)]): Unit = {
		import org.scalaml.plots.{LinePlot, LightPlotTheme, Legend}
	  		
		val legend = Legend( 
			name, "Single linear regression stock price", "Trading sessions", "Prices"
		)
		LinePlot.display(values, legend, new LightPlotTheme)
	}
	
	private def predict(
			xt: DblVector, 
			expected: DblVector, 
			slope: Double, 
			intercept: Double): Vector[DblPair] = 

		xt.view.zip(expected.view).map{ case( x, y) => (slope*x + intercept, y) }.toVector

		
	private def tabulate(
			xt: DblVector, 
			expected: DblVector, 
			slope: Double, 
			intercept: Double): String = 
			
		predict(xt, expected, slope, intercept).map{ case (p, e) => 
				s"\n${slope*p + intercept}, $e" }.mkString("\n")

	
	private def mse(
			xt: DblVector, 
			expected: DblVector, 
			slope: Double, 
			intercept: Double): Double = {
		val predicted = xt.map( slope*_ + intercept)
		XTSeries.mse(predicted, expected)
	}
}


// ----------------------------  EOF ----------------------------------
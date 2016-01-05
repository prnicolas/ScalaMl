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
package org.scalaml.app.chap3

import scala.util.{Random, Try, Success, Failure}
import scala.annotation.implicitNotFound

import org.apache.log4j.Logger

import org.scalaml.stats.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.filtering.kalman.{DKalman, QRNoise}
import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import org.scalaml.util.{DisplayUtils, FormatUtils,  LoggingUtils}
import org.scalaml.app.Eval
import LoggingUtils._, DisplayUtils._, YahooFinancials._, ScalaMl._, XTSeries._, FormatUtils._


		/**
		 * Class to evaluate the Kalman filter algorithm on a time series using
		 * a simple 2-step lag formulation
		 * {{{
		 * x(t+1) = alpha.x(t) +(1-alpha).x(t-1)<br>
		 * x(t) = x(t)
		 * }}}
		 * 
		 * @author Patrick Nicolas
		 * @since February 10, 2014
		 * @version 0.98.2
		 * @see org.scalaml.filtering
		 * @see Scala for Machine Learning Chapter 3 ''Data pre-processing'' / Kalman filter
		 */
@implicitNotFound("Kalman filter require implicit conversion Double to String")
object DKalmanEval extends FilteringEval {
 	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "DKalmanEval"
	private val OUTPUT_FILE = "output/chap3/kalman"
	private val RESOURCE_DIR = "resources/data/chap3/"
	private val NUM_VALUES = 128
	  
     
		// Noise has to be declared implicitly
	implicit val qrNoise = new QRNoise((0.7, 0.3), (m: Double) => m*Random.nextGaussian)   
		// Contract extractor
	private val extractor = YahooFinancials.adjClose :: List[Array[String] =>Double]()
	
		/**
		 * Execution of the scalatest for '''DKalman''' class 
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
	override protected def run(args: Array[String]): Int = {
		import scala.language.postfixOps
		import YahooFinancials._
	   
		require(!args.isEmpty, s"$name Command line: DKalmanEval ticker-symbol")
     
		show(s"$header Evaluation Kalman filter with no control matrix")
     
			// H and P0 are the only components that are independent from
			// input data and smoothing factor. The control matrix B is not defined
			// as there is no external control on the time series.
		val H: DblMatrix = ((0.9, 0.0), (0.0, 0.1))
		val P0: DblMatrix = ((0.4, 0.3), (0.5, 0.4))

		/**
		 * Inner function that updates the parameters/matrices for a two-step lag
		 * Kalman filter.
		 */
		def twoStepLagSmoother(xSeries: DblVector, alpha: Double): Int = {
			require(alpha > 0.0 && alpha < 1.0, s"Smoothing factor $alpha is out of range")
    	 
			// Generates the A state transition matrix from the times series updating equation
			val A: DblMatrix = ((alpha, 1.0-alpha), (0.9, 0.1))
			
			// The control matrix is null
			val B: DblMatrix =  ((0.0, 0.0), (0.0, 0.0))

			// Generate the state as a time series of pair [x(t+1), x(t)]
			val xt: Vector[(Double, Double)] = zipWithShift(xSeries, 1)
			val DISPLAY_LENGTH = 20
			show(s"First $DISPLAY_LENGTH data points x[t+1] - x[t]\n${xt.take(DISPLAY_LENGTH).mkString("\n")}")

			val pfnKalman = DKalman(A, H, P0) |>

			// Applied the Kalman smoothing for [x(t+1), x(t)]
			pfnKalman(xt) match {
				case Success(filtered) => {
					// Dump results in output file along the original time series
					val output = s"${OUTPUT_FILE}_${alpha.toString}.csv"
					val results = filtered.map(_._1)

					// Illustration of usage of the data sink
					DataSink[Double](output) |>  results :: xSeries :: List[DblVector]()

					// For convenience, on the first NUM_VALUES are plotted
					val displayedResults = results.take(NUM_VALUES)
					display(xSeries, results, alpha)
					
					// Formatted rsults
					val result = format(displayedResults.toVector,
						s"2-step lag smoother first $NUM_VALUES values", LONG)
					show(s"results $result\nCompleted")
				}
				case Failure(e) => error(s"DKalman failed with ${e.toString}")
			}
		}
      
		import org.scalaml.util.DisplayUtils._
	
		val symbol = args.head
		val source = DataSource(s"$RESOURCE_DIR$symbol.csv", false)

		  // Evaluate two different step lag smoothers.
		source.get(adjClose).map( zt => {
			twoStepLagSmoother(zt, 0.4)
			twoStepLagSmoother(zt, 0.7)
		})
		1
	}

	  /*
	   * Ubiquitous method to plot two single variable time series using
	   * org.scalaml.plot.LinePlot class
	   */
	private def display(z: DblVector, x: DblVector, alpha: Double): Unit =   {
		import org.scalaml.plots.{LinePlot, LightPlotTheme, Legend}
		
		val labels = Legend( 
			name, s"Kalman filter alpha = $alpha", s"Kalman with alpha $alpha", "y"
		)
		val data = (z, "price") :: (x, "Filtered") :: List[(DblVector, String)]()
		LinePlot.display(data, labels, new LightPlotTheme)
	}
}

// --------------------------------------  EOF -------------------------------
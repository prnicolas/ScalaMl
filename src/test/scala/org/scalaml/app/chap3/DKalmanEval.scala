/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.app.chap3

import scala.util.{Random, Try, Success, Failure}
import scala.annotation.implicitNotFound

import org.apache.log4j.Logger

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.filtering.{DKalman, QRNoise}
import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import org.scalaml.util.{DisplayUtils, FormatUtils}
import org.scalaml.app.Eval

		/**
		 * <p>Class to evaluate the Kalman filter algorithm on a time series using
		 * a simple 2-step lag formulation<br>
		 * x(t+1) = alpha.x(t) +(1-alpha).x(t-1)<br>
		 * x(t) = x(t)<br>
		 * 
		 * @author Patrick Nicolas
		 * @since February 10, 2014
		 * @note Scala for Machine Learning
		 */
@implicitNotFound("Kalman filter require implicit conversion Double to String")
object DKalmanEval extends FilteringEval {
	import YahooFinancials._, ScalaMl._
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "DKalmanEval"
	private val OUTPUT_FILE = "output/chap3/kalman"
	private val RESOURCE_DIR = "resources/data/chap3/"
	private val NUM_DISPLAYED_VALUES = 128
	  
     
		// Noise has to be declared implicitly
	implicit val qrNoise = new QRNoise((0.7, 0.3), (m: Double) => m*Random.nextGaussian)   
		// Contract extractor
	private val extractor = YahooFinancials.adjClose :: List[Array[String] =>Double]()
	
		/**
		 * <p>Execution of the scalatest for <b>DKalman</b> class 
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	override def run(args: Array[String]): Int = {
		require(!args.isEmpty, s"$name Command line DKalmanEval ticker symbol")
     
		DisplayUtils.show(s"$header Evaluation Kalman filter with no control matrix", logger)
     
			// H and P0 are the only components that are independent from
			// input data and smoothing factor. The control matrix B is not defined
			// as there is no external control on the time series.
		val H: DblMatrix = ((0.9, 0.0), (0.0, 0.1))
		val P0: DblMatrix = ((0.4, 0.3), (0.5, 0.4))
     
		/**
		 * Inner function that updates the parameters/matrices for a two-step lag
		 * Kalman filter.
		 */
		def twoStepLagSmoother(zSeries: DblVector, alpha: Double): Int = {
			require(alpha > 0.0 && alpha < 1.0, s"$name smoothing factor $alpha is out of range")
    	 
			// Generates the A state transition matrix from the times series updating equation
			val A: DblMatrix = ((alpha, 1.0-alpha), (1.0, 0.0))

			// Generate the state as a time series of pair [x(t+1), x(t)]
			val zt_1 = zSeries.drop(1)
			val zt = zSeries.dropRight(1)
         
			// Applied the Kalman smoothing for [x(t+1), x(t)]
			val filtered = DKalman(A, H, P0) |> XTSeries[(Double, Double)](zt_1.zip(zt))
	     
			// Dump results in output file along the original time series
			val output = s"${OUTPUT_FILE}_${alpha.toString}.csv"
			val results: XTSeries[Double] = filtered.map(_._1)
			DataSink[Double](output) |> results :: XTSeries[Double](zSeries) :: List[XTSeries[Double]]()
			val displayedResults: DblVector = results.toArray.take(NUM_DISPLAYED_VALUES)
			
			display(zSeries, results.toArray, alpha)
			val result = FormatUtils.format(displayedResults, 
					s"2-step lag smoother first $NUM_DISPLAYED_VALUES values", FormatUtils.LongFormat)
			DisplayUtils.show(s"$name results $result", logger)

		}
      
		Try {
			val symbol = args(0)
			val source = DataSource(s"${RESOURCE_DIR}${symbol}.csv", false)
			val zt = (source |> YahooFinancials.adjClose).toArray  

			twoStepLagSmoother(zt, 0.5)
			twoStepLagSmoother(zt, 0.8)
		} 
		match {
			case Success(n) => n
			case Failure(e) => failureHandler(e)
		}
	}
	
	private def display(z: DblVector, x: DblVector, alpha: Double): Unit =   {
		import org.scalaml.plots.{LinePlot, LightPlotTheme}
		
		val labels = List[String]( 
			name, "Kalman filter", s"Kalman with alpha $alpha", "y"
		)
		val data = (z, "price") :: (x, "Filtered") :: List[(DblVector, String)]()
		LinePlot.display(data, labels, new LightPlotTheme)
	}
}


// --------------------------------------  EOF -------------------------------
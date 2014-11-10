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
package org.scalaml.app.chap3

import scala.util.{Random, Try, Success, Failure}
import org.scalaml.core.types
import org.scalaml.filtering.{DKalman, QRNoise}
import org.scalaml.core.XTSeries
import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import scala.annotation.implicitNotFound
import org.apache.log4j.Logger
import org.scalaml.util.Display

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
class DKalmanEval extends FilteringEval {
   import YahooFinancials._, types.ScalaMl._
   val name: String = "DKalmanEval"
   
   val logger = Logger.getLogger(name)
     
   		  // Noise has to be declared implicitly
   implicit val qrNoise = QRNoise((0.7, 0.3), (m: Double) => m*Random.nextGaussian)   
   		// Contract extractor
   val extractor = YahooFinancials.adjClose :: List[Array[String] =>Double]()

   override def run(args: Array[String]): Int = {
     require(args != null && args.size > 0, "Command line DKalmanEval ticker symbol")
     
     Display.show("Kalman filter evaluation", logger)
     
     	// H and P0 are the only components that are independent from
        // input data and smoothing factor. The control matrix B is not defined
        // as there is no external control on the time series.
     val H: DblMatrix = ((0.9, 0.0), (0.0, 0.1))
	 val P0: DblMatrix = ((0.4, 0.3), (0.5, 0.4))
     
	 	/**
	 	 * Inner function that updates the parameters/matrices for a two-step lag
	 	 * Kalman filter.
	 	 */
	 def twoStepLagSmoother(zSeries: DblVector, alpha: Double): Unit = {  
    	 require(alpha > 0.0 && alpha < 1.0, s"Kalman smoothing factor $alpha is out of range")
    	 
    	 	// Generates the A state transition matrix from the times series updating equation
         val A: DblMatrix = ((alpha, 1.0-alpha), (1.0, 0.0))
         	// Generate the state as a time series of pair [x(t+1), x(t)]
         val zt_1 = zSeries.drop(1)
         val zt = zSeries.take(zSeries.size-1)
         
         	// Applied the Kalman smoothing for [x(t+1), x(t)]
	     val filtered = DKalman(A, H, P0) |> XTSeries[(Double, Double)](zt_1.zip(zt))
	     
	     	// Dump results in output file along the original time series
	     val output = s"output/chap3/kalman_${alpha.toString}.csv"
	     DataSink[Double](output) |> filtered.map(_._1) :: XTSeries[Double](zSeries) :: List[XTSeries[Double]]() 
      }
      
      Try {
	      val symbol = args(0)
	      val source = DataSource("resources/data/chap3/" + symbol + ".csv", false)
	      val zt = (source |> YahooFinancials.adjClose).toArray  

		  twoStepLagSmoother(zt, 0.5)
	      twoStepLagSmoother(zt, 0.8)
		  Display.show("DKalmanEval.run completed", logger)
      } match {
      	case Success(n) => n
      	case Failure(e) => Display.error("DKalmanEval.run", logger, e)
      }
      
   }
}

object DKalmanEval {
	def apply(implicit f: Double=> String): DKalmanEval = new DKalmanEval
}


// --------------------------------------  EOF -------------------------------
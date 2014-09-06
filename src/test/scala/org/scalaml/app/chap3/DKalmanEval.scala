/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.app.chap3

import scala.util.Random

import org.scalaml.core.Types
import org.scalaml.filtering.{DKalman, QRNoise}
import org.scalaml.core.XTSeries
import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import scala.annotation.implicitNotFound

		/**
		 * Class to evaluate the Kalman filter algorithm.
		 * @param f implicit conversion of a double to a string
		 * 
		 * @author Patrick Nicolas
		 * @since February 10, 2014
		 * @note Scala for Machine Learning
		 */
@implicitNotFound("Kalman filter require implicit conversion Double to String")
class DKalmanEval(implicit f: Double=> String) extends FilteringEval {
   import YahooFinancials._, Types.ScalaMl._
  

   implicit val qrNoise = QRNoise((0.7, 0.9), (m: Double) => m* Random.nextGaussian)   
   val extractor = YahooFinancials.adjClose :: List[Array[String] =>Double]()

   		/**
   		 * Implements the run/evaluation function for the Kalman filter.
   		 */
   override def run(args: Array[String]): Unit = {
     import Types.ScalaMl._
     require(args != null && args.size > 0, "Command line DKalmanEval ticker symbol")
     
     Console.println("Kalman filter evaluation")
     val B: DblMatrix = ((0.0), (0.0))
     val H: DblMatrix = (1.0, 1.0)
	 val P0: DblMatrix = ((0.4, 0.4), (0.4, 0.4))
	 val x0: DblVector = (5.0, 0.0)
     
	 	/**
	 	 * Inner function that updates the parameters/matrices 
	 	 */
     def computeKalman(zSeries: DblVector, index: Int, x1: Double, x2: Double, y1: Double, y2:Double): Unit = {
         def A = Array[Array[Double]](Array[Double](x1, x2), Array[Double](y1, y2))
         
         val dKalman = new DKalman(A, B, H, P0)
         val zt_1 = zSeries.drop(1)
         val zt = zSeries.take(zSeries.size-1)
         val output = "output/chap3/kalman2_" + index + ".csv"
         
	     dKalman |> XTSeries[(Double, Double)](zt_1.zip(zt)) match {
	        case Some(filtered) => DataSink[Double](output) |> filtered.map(_._1) :: List[XTSeries[Double]]() 
	        case None => Console.println("Kalman filter failed")
	     }
      }
      
      val symbol = args(0)
      val source = DataSource("resources/data/chap3/" + symbol + ".csv", false)
      source |> YahooFinancials.adjClose match {
          case Some(zt) => {  
		       val testx1 = Array[Double](0.5)
		       val testx2 = Array[Double](0.5)
		    
		       computeKalman(zt, 0, 0.5, 0.5, 0.1, 0.9)
		       computeKalman(zt, 1, 0.5, 0.5, 0.9, 0.1)
          }
          case None => Console.println("Could not complete Kalman computation")
      }
   }
}

object DKalmanEval {
	def apply(implicit f: Double=> String): DKalmanEval = new DKalmanEval
}


// --------------------------------------  EOF -------------------------------
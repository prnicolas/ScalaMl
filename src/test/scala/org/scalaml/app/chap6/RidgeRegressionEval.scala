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
package org.scalaml.app.chap6

import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.supervised.regression.linear.RidgeRegression
import org.scalaml.util.{DisplayUtils, FormatUtils}
import org.scalaml.app.Eval

		/**
		 * <p><b>Purpose:</b> Singleton to evaluate the Ridge regression classifier on 
		 * technical analysis of an Exchange Traded Fund.</p>
		 * 
		 * @author Patrick Nicolas
		 * @note: Scala for Machine Learning  Chapter 6 Regression and regularization / Ridge regression
		 */
object RidgeRegressionEval extends Eval {
	import scala.util.{Try, Success, Failure}
	import org.apache.log4j.Logger
	import ScalaMl._, YahooFinancials._
  
		/**
		 * Name of the evaluation 
		 */
	val name: String = "RidgeRegressionEval"
	
	private val path = "resources/data/chap6/CU.csv"
	private val dataInput = "output/chap6/CU_input.csv"
	
		 /**
		 * <p>Execution of the scalatest for <b>RidgeRegression</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Evaluation of Ridge regression", logger)
  	   	 
		Try {
			val src = DataSource(path, true, true, 1)
			val price = src |> YahooFinancials.adjClose
			val volatility = src |> YahooFinancials.volatility 
			val volume = src |> YahooFinancials.volume
		
			val deltaPrice = XTSeries[Double](price.drop(1).zip(price.dropRight(1))
															.map( z => z._1 - z._2))
		  /*
			DataSink[Double](dataInput) |> deltaPrice :: 
										volatility :: 
										volume :: List[XTSeries[Double]]()
										* 
										*/
			val data =  volatility.zip(volume)
									.map(z => Array[Double](z._1, z._2))
		
			val features = XTSeries[DblVector](data.dropRight(1))
			val regression = new RidgeRegression[Double](features, deltaPrice, 0.5)

				// Retrieve the weights or coefficient of the Ridge regression
			regression.weights.map(w =>  {
				w.zipWithIndex.foreach( wi => {
					val weights_str = FormatUtils.format(wi._2, ": ", FormatUtils.ShortFormat)
					DisplayUtils.show(s"$name ${wi._1}$weights_str", logger)
				})
			}).getOrElse(DisplayUtils.error(s"$name Ridge regression could not be trained", logger))

				// Retrieve the residual sum of squared error of the Ridge regression
			regression.rss.map( rss => {
				val result = FormatUtils.format(rss, "rss =", FormatUtils.MediumFormat)
				DisplayUtils.show(s"$name $result", logger)
			}).getOrElse(DisplayUtils.error(s"$name Ridge regression could not be trained", logger))

				// Create two predictor 
			val y1 = predict(0.2, deltaPrice, volatility, volume)
			val y2 = predict(5.0, deltaPrice, volatility, volume)
			display(deltaPrice, y1, y2, 0.2, 5.0)
			
			assert( regression.isModel, "Ridge regression mode is incomplete")
			if( regression.isModel ) {
				(2 until 10 by 2).foreach( n => { 
					val lambda = n*0.1
					val y = predict(lambda, deltaPrice, volatility, volume)
					DisplayUtils.show(s"Lambda  $lambda", logger )
					DisplayUtils.show(FormatUtils.format(y, "", FormatUtils.ShortFormat), logger)
				})
			}
			DisplayUtils.show(s"$name.run Regression succeeded", logger)
		} match {
			case Success(n) => n
			case Failure(e) => failureHandler(e)
		}
 	}
   
 	private def rss(
 			lambda: Double, 
 			deltaPrice: DblVector, 
 			volatility: DblVector, 
 			volume: DblVector): Double = {
		val data =  volatility.zip(volume).map(z => Array[Double](z._1, z._2))
		
		val features = XTSeries[DblVector](data.dropRight(1))
		val regression = new RidgeRegression[Double](features, deltaPrice, lambda)
		regression.rss.get
	}
   
	private def predict(
			lambda: Double, 
			deltaPrice: DblVector, 
			volatility: DblVector, 
			volume: DblVector): DblVector = {
		val data =  volatility.zip(volume).map(z => Array[Double](z._1, z._2))
		
		val features = XTSeries[DblVector](data.dropRight(1))
		val regression = new RidgeRegression[Double](features, deltaPrice, lambda)
		features.map( regression |> _)
 	}
	
	private def display(
			z: DblVector, 
			y1: DblVector, 
			y2: DblVector, 
			lambda1: Double, 
			lambda2: Double): Unit = {
		import org.scalaml.plots.{LinePlot, LightPlotTheme}
	  
		val labels = List[String]( 
			name, "Ridge Regression", s" L2 lambda impact", "y"
		)
		val data = (z, "Delta price") :: 
						(y1, s"L2 lambda $lambda1") :: 
						(y2, s"L2 lambda $lambda2") :: List[(DblVector, String)]()
					
		LinePlot.display(data, labels, new LightPlotTheme)
	}
}

// ----------------------------  EOF ----------------------------------
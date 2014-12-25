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

import org.scalaml.trading.YahooFinancials
import org.scalaml.core.XTSeries
import org.scalaml.core.XTSeries.{DblSeries, DblVecSeries}
import org.scalaml.core.Types.ScalaMl.{DblVector, DblMatrix, XY}
import org.scalaml.workflow.data.{DataSink, DataSource}
import org.scalaml.supervised.regression.linear.MultiLinearRegression
import org.scalaml.filtering.SimpleMovingAverage
import org.scalaml.util.{FormatUtils, DisplayUtils}
import org.scalaml.app.Eval


		/**
		 * <p><b>Purpose:</b> Singleton to test the multi-variate least squares regression. The evaluation
		 * is composed of two tests<br>
		 * Trend analysis and filter: MultiLinearRegressionEval.filter<br>
		 * Features selection: MultiLinearRegressionEval.featuresSelection.</p>
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning Chapter 6 Regression and regularization/Ordinary least 
		 * squares regression 
		 */
object MultiLinearRegressionEval extends Eval {
	import scala.collection.mutable.ListBuffer	
	import scala.util.{Try, Success, Failure}
	import org.apache.log4j.Logger
	import YahooFinancials._

		/**
		 * Name of the evaluation 
		 */
	val name: String = "MultiLinearRegressionEval"
 
		/**
		 * <p>Execution of the scalatest for <b>MultiLinearRegression</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = 
		if( !args.isEmpty && args(0).equals("trending")) 
			trendExtraction 
		else
			featuresSelection
   
	private def featuresSelection: Int = {
		DisplayUtils.show(s"\n$header Ordinary least squares regression FEATURE SELECTION", logger)
  	  
		val path = "resources/data/chap6/"
		val output = "output/chap6/CNY_output.csv"
		val symbols = Array[String]("CNY", "GLD", "SPY", "TLT")
		val smoothingPeriod = 16

		val movAvg = SimpleMovingAverage[Double](smoothingPeriod)
		Try {
			val input = symbols.map(s => DataSource(path + s +".csv", true, true, 1))
							.map( _ |> YahooFinancials.adjClose )
							.map(x => movAvg |> XTSeries[Double](x.slice(20, 800)))
	  	    
			val sinkInput: List[DblSeries] = input.foldLeft(List[DblSeries]())((sk, v) => v :: sk)
			DataSink[Double](output) |> sinkInput
	  	  
			// Retrieve the input variables by removing the first 
			// time series (labeled dataset) and transpose the array
			val features = input.drop(1)
			val featuresList = List[(Array[String], DblMatrix)](
				(Array[String]("CNY", "SPY" , "GLD", "TLT"), features.map( _.toArray).transpose),
				(Array[String]("CNY", "GLD", "TLT"), features.drop(1).map( _.toArray).transpose),
				(Array[String]("CNY", "SPY", "GLD"), features.take(2).map( _.toArray).transpose),
				(Array[String]("CNY", "SPY", "TLT"), features.zipWithIndex.filter( _._2 != 1)
																		.map( _._1.toArray)
																		.transpose),
				(Array[String]("CNY", "GLD"), features.slice(1,2).map( _.toArray).transpose)
			)
	  	  		
			featuresList.foreach(ft => 
					DisplayUtils.show(s"${getRss(XTSeries[DblVector](ft._2), input(0), ft._1)}", logger))  	  

				// Compute the mean square error for each solution.
			val errors = featuresList.map(ft => rssSum(XTSeries[DblVector](ft._2), input(0))._1)
			val tss = Math.sqrt(errors.sum)/featuresList.size

			Range(0, featuresList.size).foreach(n => 
					DisplayUtils.show(s"MSE for ${featuresList(n)._1.mkString(" ")} ${errors(n)}", logger))
			DisplayUtils.show(s"\n$name Residual error $tss", logger)
		} 
		match {
			case Success(n) => n
			case Failure(e) =>failureHandler(e)
		}
	}
  	  
 	private def getRss(xt: DblVecSeries, y: DblVector, featureLabels: Array[String]): String = {
		val regression = MultiLinearRegression[Double](xt, y)
		val buf = new StringBuilder
		
		regression.weights.get.zipWithIndex.foreach(w => {
			val weights_str = FormatUtils.format(w._1, "", FormatUtils.ShortFormat)
			if( buf.length < 2)  
				buf.append(s"${featureLabels(w._2)} = $weights_str")
			else  {
				buf.append(s" + ${weights_str}.${featureLabels(w._2)}")
			}
		})
		buf.append(s"\n$name RSS: ${regression.rss.get}").toString
	}
  

	private def rssSum(xt: DblVecSeries, y: DblVector): XY = {
		val regression = MultiLinearRegression[Double](xt, y)
		val rss = regression.rss.get
		val arr: DblMatrix = xt.toArray
		val results = arr.zip(y).foldLeft(0.0)((s, x) => {
			val diff = (x._2 - (regression |> x._1))
			s + diff*diff
		})
		
		(regression.rss.get, results)
	}
  
  	 
	private def trendExtraction: Int = {
		DisplayUtils.show(s"$header Ordinary least squares regression TRENDING", logger)
  	   	 
		val path = "resources/data/chap6/CU.csv"
		val output = "output/chap6/CU_output.csv"
  	 
		Try {
			val src = DataSource(path, true, true, 1)
			val prices = (src |> YahooFinancials.adjClose).toArray 
			val volatility = src |> YahooFinancials.volatility 
			val volume = src |> YahooFinancials.volume
	
			val deltaPrice: DblVector = prices.drop(1)
									.zip(prices.dropRight(1))
									.map(z => z._1 - z._2)
	    
			DataSink[Double](output) |>  XTSeries[Double](deltaPrice) ::
										volatility :: 
										volume :: 
										List[DblSeries]()

			val data =  volatility.zip(volume)
									.map(z => Array[Double](z._1, z._2))

				// Features are volatility and volume
			val features = XTSeries[DblVector](data.dropRight(1))
			val regression = MultiLinearRegression[Double](features, deltaPrice)

			regression.weights.map(w => {
				val buf = new StringBuilder(s"$name Multi-regression weights\n")
					
				w.zipWithIndex.foreach( wi => buf.append(s"${wi._1}${wi._2} "))
				DisplayUtils.show(buf.toString, logger)
					
				DisplayUtils.show(deltaPrice.toSeq, logger)
				
				val trend = data.map( vv => w(0) + vv(0)*w(1) + vv(1)*w(1) )
				DisplayUtils.show("trend", logger)
				DisplayUtils.show(trend, logger)
				display(deltaPrice, trend)
			}).getOrElse(DisplayUtils.error(s"$name Multivariate regression could not be trained", 
					logger))

			DisplayUtils.show(s"$name.filter Completed", logger)
		}
  	 
		match {
			case Success(n) => n
			case Failure(e) => DisplayUtils.error(s"$name filter failed", logger, e)
		}
	}
	
	private def display(z: DblVector, x: DblVector): Unit =   {
		import org.scalaml.plots.{LinePlot, LightPlotTheme}
		
		val labels = List[String]( 
			name, "Multi line regression", s"Raw vs. filtered", "y"
		)
		val data = (z, "Delta price") :: (x, "Filtered") :: List[(DblVector, String)]()
		LinePlot.display(data, labels, new LightPlotTheme)
	}
}

// ----------------------------  EOF ----------------------------------
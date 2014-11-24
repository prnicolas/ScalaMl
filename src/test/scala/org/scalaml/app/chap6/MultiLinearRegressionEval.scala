/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.app.chap6

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.YahooFinancials
import YahooFinancials._
import org.scalaml.core.XTSeries
import org.scalaml.workflow.data.DataSink
import org.scalaml.supervised.regression.linear.MultiLinearRegression
import org.scalaml.core.types.ScalaMl
import org.scalaml.core.types.ScalaMl._
import org.scalaml.filtering.SimpleMovingAverage
import org.scalaml.util.Display
import org.apache.log4j.Logger
import scala.collection.mutable.ListBuffer
import scala.util.{Try, Success, Failure}
import org.scalaml.app.Eval


			/**
			 * <p>Singleton to test the multi-variate least squares regression. The evaluation
			 * is composed of two tests<br>
			 * Trend analysis and filter: MultiLinearRegressionEval.filter<br>
			 * Features selection: MultiLinearRegressionEval.featuresSelection
			 */
object MultiLinearRegressionEval extends Eval {
	val name: String = "MultiLinearRegressionEval"
	private val logger = Logger.getLogger(name)
 
		/**
		 * <p>Execution of the scalatest for <b>MultiLinearRegression</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String] = null): Int = 
		if( args != null && args.size > 0 && args(0).equals("trending")) 
			trendExtraction 
			
		else
			featuresSelection	  		 
   
	private def featuresSelection: Int = {
		Display.show(s"\n$name Ordinary least squares regression FEATURE SELECTION", logger)
  	  
		val path = "resources/data/chap6/"
		val output = "output/chap6/CNY_output.csv"
		val symbols = Array[String]("CNY", "GLD", "SPY", "TLT")
		val smoothingPeriod = 16

		val movAvg = SimpleMovingAverage[Double](smoothingPeriod)
		Try {
			val input = symbols.map(s => DataSource(path + s +".csv", true, true, 1))
							.map( _ |> YahooFinancials.adjClose )
							.map(x => movAvg |> XTSeries[Double](x.slice(20, 800)))
	  	    
			DataSink[Double](output) |> input.foldLeft(List[XTSeries[Double]]())((sk, v) => v :: sk)
	  	  
			// Retrieve the input variables by removing the first 
			// time series (labeled dataset) and transpose the array
			val features = input.drop(1)
			val featuresList = List[(Array[String], DblMatrix)](
				(Array[String]("CNY", "SPY" , "GLD", "TLT"), features.map( _.toArray).transpose),
				(Array[String]("CNY", "GLD", "TLT"), features.drop(1).map( _.toArray).transpose),
				(Array[String]("CNY", "SPY", "GLD"), features.take(2).map( _.toArray).transpose),
				(Array[String]("CNY", "SPY", "TLT"), features.zipWithIndex.filter( _._2 != 1).map( _._1.toArray).transpose),
				(Array[String]("CNY", "GLD"), features.slice(1,2).map( _.toArray).transpose)
			)
	  	  		
			featuresList.foreach(ft => 
				Display.show(s"${getRss(XTSeries[DblVector](ft._2), input(0), ft._1)}", logger ))  	  

				// Compute the mean square error for each solution.
			val errors = featuresList.map(ft => rssSum(XTSeries[DblVector](ft._2), input(0))._1)
			val tss = Math.sqrt(errors.sum)/featuresList.size

			Range(0, featuresList.size).foreach(n => {
				val featureLabel = featuresList(n)._1.foldLeft(new StringBuilder)((b, symbol) => b.append(s"$symbol ")).toString
				Display.show(s"MSE for ${featureLabel} ${errors(n)}", logger )
			})

			Display.show(s"\n$name Residual error $tss", logger)
		} 
		match {
			case Success(n) => n
			case Failure(e) => Display.error(s"$name Inference test", logger, e)
		}
	}
  	  
 	private def getRss(xt: XTSeries[DblVector], y: DblVector, featureLabels: Array[String]): String = {
		val regression = MultiLinearRegression[Double](xt, y)
		val buf = new StringBuilder
		
		regression.weights.get.zipWithIndex.foreach(w => {
			if( buf.length < 2) 
				buf.append(s"${featureLabels(w._2)}=${ScalaMl.toString(w._1, "", true)}")
			else 
				buf.append(s" + ${ScalaMl.toString(w._1, "", true)}.${featureLabels(w._2)}")
		})
		buf.append(s"\n$name RSS: ${regression.rss.get}").toString
	}
  

	private def rssSum(xt: XTSeries[DblVector], y: DblVector): XY = {
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
		Display.show(s"\n** test#${Eval.testCount} $name Ordinary least squares regression TRENDING", logger)
  	   	 
		val path = "resources/data/chap6/CU.csv"
		val output = "output/chap6/CU_output.csv"
  	 
		Try {
			val src = DataSource(path, true, true, 1)
			val prices = (src |> YahooFinancials.adjClose).toArray 
			val volatility = src |> YahooFinancials.volatility 
			val volume = src |> YahooFinancials.volume
	
			val deltaPrice = prices.drop(1)
									.zip(prices.dropRight(1))
									.map(z => z._1 - z._2)
	    
			DataSink[Double](output) |>  XTSeries[Double](deltaPrice) ::
											volatility :: 
											volume :: 
											List[XTSeries[Double]]()

			val data =  volatility.zip(volume)
									.map(z => Array[Double](z._1, z._2))
		    
			val features = XTSeries[DblVector](data.dropRight(1))
			val regression = MultiLinearRegression[Double](features, deltaPrice)

			regression.weights match {
				case Some(w) => {
					val buf = new StringBuilder(s"$name.filter:\n")
					w.zipWithIndex.foreach( wi => buf.append(s"${wi._1}${wi._2} "))
					Display.show(buf.toString, logger)
				}
				case None => Display.error(s"$name Multivariate regression could not be trained", logger)
			}
			Display.show(s"$name.filter Completed", logger)
		}
  	 
		match {
			case Success(n) => n
			case Failure(e) => Display.error(s"$name filter failed", logger, e)
		}
	}
}


// ----------------------------  EOF ----------------------------------
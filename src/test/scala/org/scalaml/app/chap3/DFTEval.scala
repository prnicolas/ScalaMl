/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95d
 */
package org.scalaml.app.chap3

import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.core.XTSeries
import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import org.scalaml.util.{FormatUtils, DisplayUtils}
import org.scalaml.filtering.{DFTFir, DFT, DTransform}
import org.scalaml.app.Eval


		/**
		 * <p><b>Purpose</b>: Evaluate the Discrete Sine and Cosine Fourier transform.</p>
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning  Chapter 3 Data pre-processing / Discrete Fourier transform
		 */
object DFTEval extends FilteringEval {	
	import scala.util.{Try, Success, Failure}
	import org.apache.log4j.Logger
	import YahooFinancials._

		/**
		 * Name of the evaluation 
		 */
	val name: String = "DFTEval"  
		/**
		 * Maximum duration allowed for the execution of the evaluation
		 */
    val maxExecutionTime: Int = 25000
    
	private val logger = Logger.getLogger(name)
	private val h = (x:Double) => 2.0*Math.cos(Math.PI*0.005*x) +  // simulated first harmonic
															Math.cos(Math.PI*0.05*x) +   // simulated second harmonic
															0.5*Math.cos(Math.PI*0.2*x)      // simulated third harmonic
		/**
		 * <p>Execution of the scalatest for <b>DFT</b> class 
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		Try {
			if( args.isEmpty) runSimulation else runFinancial(args(0))
		} 
		match {
			case Success(n) => n
			case Failure(e) => DisplayUtils.error(s"$name.run failed", logger, e)
		}
	}
   

	private def runSimulation: Int = {
		DisplayUtils.show(s"$header Discrete Fourier series with synthetic data", logger)
		val values = Array.tabulate(1025)(x => h(x/1025.0))
			// Original data dump into a CSV file
		DataSink[Double]("output/chap3/simulated.csv") write values
     
		val frequencies = DFT[Double] |> XTSeries[Double](values)
		DataSink[Double]("output/chap3/smoothed.csv") write frequencies
		
		val results = FormatUtils.format(frequencies.toArray.take(128), "x/1025", 
				FormatUtils.ShortFormat)
		DisplayUtils.show(s"$name Results simulation first 128 fequencies: ${results}", logger)
	}
   
	private def runFinancial(symbol: String): Int  = {
		DisplayUtils.show(s"$header Discrete Fourier series with financial data $symbol", logger)
		val src = DataSource("resources/data/chap3/" + symbol + ".csv", false, true, 1)

		val price = src |> YahooFinancials.adjClose
		var filtered = filter(0.01, price)
		filtered = filter(0.005, price)

		val sink2 = DataSink[Double]("output/chap3/filt_" + symbol + ".csv")
		sink2 |>  XTSeries[Double](filtered) :: List[XTSeries[Double]]()

		val result = FormatUtils.format(filtered.take(256), "DTF filtered", FormatUtils.ShortFormat)
		DisplayUtils.show(s"$name Result first 256 frequencies: $result", logger)
	}
	
	private def filter(cutOff: Double, price: XTSeries[Double]): DblVector = {
		import DTransform._
		val filter = new DFTFir[Double](sinc, cutOff)
 
		val xtSeries = filter |> price
		val filtered: DblVector = xtSeries
		display(price, filtered, cutOff)
		filtered
	}
	
	private def display(x1: DblVector, x2: DblVector, fc: Double): Unit =   {
		import org.scalaml.plots.{LinePlot, LightPlotTheme, BlackPlotTheme}
	  
		val plot = new LinePlot(("Discrete Fourier filter", s"cutoff $fc", "y"), new LightPlotTheme)
		val _x2 = x2.take(x1.size-24)
		val data = (x1, "Stock price") :: (_x2, "DFT") :: List[(DblVector, String)]()
						
		plot.display(data, 340, 280)
		val plot2 = new LinePlot(("DFT filtered noise", s"filtered noise $fc", "y"), new BlackPlotTheme)
		val data2 = x1.zip(_x2).map(z => Math.abs(z._1 - z._2))
		plot2.display(data2, 340, 280)
	}
}

// --------------------------------------  EOF -------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95d
 */
package org.scalaml.app.chap3



import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.XTSeries
import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import YahooFinancials._
import org.apache.log4j.Logger
import org.scalaml.util.Display
import scala.util.{Try, Success, Failure}
import org.scalaml.filtering.{DFTFir, DFT, DTransform}
import org.scalaml.app.Eval

		/**
		 * <p>Command line application. Singleton used to evaluate the Discrete Sine and Cosine Fourier transform.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since February 8, 2014
		 * @note Scala for Machine Learning
		 */
object DFTEval extends FilteringEval {	
	import org.scalaml.filtering.DFT

	val name: String = "DFTEval"  
   
	private val logger = Logger.getLogger(name)
	private val h = (x:Double) =>2.0*Math.cos(Math.PI*0.005*x) +  // simulated first harmonic
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
			if( args == null || args.size == 0) 
				runSimulation
			else 
				runFinancial(args(0))
		} 
		match {
			case Success(n) => n
			case Failure(e) => Display.error(s"$name.run failed", logger, e)
		}
	}
   

	private def runSimulation: Int = {
		import ScalaMl._
		Display.show(s"\n** test#${Eval.testCount} $name Discrete Fourier series with synthetic data", logger)
		val values = Array.tabulate(1025)(x => h(x/1025))
			// Original data dump into a CSV file
		DataSink[Double]("output/chap3/simulated.csv") write values
     
		val frequencies = DFT[Double] |> XTSeries[Double](values)
		DataSink[Double]("output/chap3/smoothed.csv") write frequencies
		Display.show(s"$name Results simulation (first 512 frequencies): ${ScalaMl.toString(frequencies.toArray.take(512), "x/1025", true)}", logger)
	}
   
	import DTransform._
	private def runFinancial(symbol: String): Int  = {
		import ScalaMl._
		
		Display.show(s"\n** test#${Eval.testCount} $name Discrete Fourier series with financial data $symbol", logger)
		val src = new DataSource("resources/data/chap3/" + symbol + ".csv", false, true)
     
		val price = src |> YahooFinancials.adjClose  
		val filter = new DFTFir[Double](sinc, 4.5)
         
		val xtSeries = filter |> price
		val res: DblVector = xtSeries
		val thresholdValue = res.max*0.01
		val sink2 = DataSink[Double]("output/chap3/filt_" + symbol + ".csv")
		val filtered = res.map( x => if(x > thresholdValue) x else 0.0)
		sink2 |>  XTSeries[Double](filtered) :: List[XTSeries[Double]]()
		
		Display.show(s"$name Results financial data(first 512 frequencies): ${ScalaMl.toString(filtered.take(512), "DTF filtered", true)}", logger)
	}
}

// --------------------------------------  EOF -------------------------------
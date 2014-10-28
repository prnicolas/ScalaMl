/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.94
 */
package org.scalaml.app.chap3



import org.scalaml.core.{types, XTSeries}
import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import YahooFinancials._
import org.apache.log4j.Logger
import org.scalaml.util.Display
import scala.util.{Try, Success, Failure}
import org.scalaml.filtering.{DFTFir, DFT, DTransform}

		/**
		 * <p>Command line application. Singleton used to evaluate the Discrete Sine and Cosine Fourier transform.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since February 8, 2014
		 * @note Scala for Machine Learning
		 */
object DFTEval extends FilteringEval {
   import types.ScalaMl._
   import org.scalaml.filtering.DFT
     
   val logger = Logger.getLogger("DFTEval")
   val h = (x:Double) =>2.0*Math.cos(Math.PI*0.005*x) +  // simulated first harmonic
                            Math.cos(Math.PI*0.05*x) +   // simulated second harmonic
                        0.5*Math.cos(Math.PI*0.2*x)      // simulated third harmonic
                            
   def run(args: Array[String]): Int = {
  	 Try {
	  	if( args == null && args.length == 0) 
	  		runSimulation
	  	else 
	  		runFinancial(args(0))
	  	Display.show("DFTEval.run completed", logger)
  	 } match {
  		 case Success(n) => n
  		 case Failure(e) => Display.error("DFTEval.run failed", logger, e)
  	 }
   }
   
   private def runSimulation: Unit = {
  	 Display.show("DFTEval.run Evaluation of Discrete Fourier series with synthetic data", logger)
  
  	 val values = Array.tabulate(1025)(x => h(x/1025))
  	   // Original data dump into a CSV file
     DataSink[Double]("output/chap3/simulated.csv") |> values
     
     val frequencies = DFT[Double] |> XTSeries[Double](values)
     DataSink[Double]("output/chap3/smoothed.csv") |> frequencies
  }
   
   import DTransform._
   private def runFinancial(symbol: String): Unit  = {
     Display.show("DFTEval.run Evaluation of Discrete Fourier series with financial data", logger)
     val src = new DataSource("resources/data/chap3/" + symbol + ".csv", false, true)
     
     val price = src |> YahooFinancials.adjClose  
     val filter = new DFTFir[Double](sinc, 4.5)
         
     val xtSeries = filter |> price
     val res: DblVector = xtSeries
     val thresholdValue = res.max*0.01
     val sink2 = DataSink[Double]("output/chap3/filt_" + symbol + ".csv")
     sink2 |>  XTSeries[Double](res.map( x => if(x > thresholdValue) x else 0.0)) :: List[XTSeries[Double]]()
   }
}

// --------------------------------------  EOF -------------------------------
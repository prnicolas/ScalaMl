/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 */
package org.scalaml.app.chap1

import org.scalaml.core.types
import org.scalaml.stats.Stats
import scala.io.Source
import org.scalaml.plots._
import org.apache.log4j.Logger
import scala.util.{Try, Success, Failure}
import org.scalaml.util.Display
import org.scalaml.app.Eval

		/**
		 * <p>Singleton to evaluate the different plotting format used in
		 * the library.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since December 22, 2013
		 * @note Scala for Machine Learning
		 */
object PlotterEval extends Eval {
	val name: String = "PlotterEval"
		
	final val CSV_DELIM = ","
	final val PRICE_COLUMN_INDEX = 6
	final val OPEN_COLUMN_INDEX = 1
	final val VOL_COLUMN_INDEX = 5
	final val HIGH_INDEX = 2
	final val LOW_INDEX = 3
    final val pathName = "resources/data/chap1/CSCO.csv"
				
    private val logger = Logger.getLogger(name)
	def run(args: Array[String]): Int = {
		Display.show(s"\n$name Evaluation of JFreeChart library", logger)
		
		Try {
			val src = Source.fromFile(pathName)
			val fields = src.getLines.map( _.split(CSV_DELIM)).toArray	   
			
			val cols = fields.drop(1)
			val volatility = Stats[Double]( cols.map( f => f(HIGH_INDEX).toDouble - f(LOW_INDEX).toDouble ) ).normalize
			val normVolume =  Stats[Double](cols.map( _(VOL_COLUMN_INDEX).toDouble) ).normalize
			val volatility_volume: Array[(Double, Double)] = volatility.zip(normVolume)
			
			Display.show(s"$name Line plot for CSCO stock normalized volume", logger)
			val theme1 = new LightPlotTheme
			val plotter1 = new LinePlot((s"$name: CSCO 2012-2013 Stock", "Volume", "r"), theme1)
			plotter1.display(normVolume, 200, 200, 1)
			
			Display.show(s"$name Line plot for CSCO stock volatility", logger)
			val theme2 = new BlackPlotTheme
			val plotter2 = new LinePlot((s"$name: CSCO 2012-2013 Stock", "Volatility", "r"), theme2)
			plotter2.display(volatility, 300, 100, 1)
		    	
			Display.show(s"$name Scatter plot CSCO stock volatility vs. volume", logger)
			val theme3 = new LightPlotTheme
			val plotter3 = new ScatterPlot((s"$name: CSCO 2012-2013 Stock", "Volatility vs. Volume", "r"), theme3)
			plotter3.display(volatility_volume, 200, 200)
		    
			src.close
		} 
		match {
			case Success(n) => Display.show(s"$name Test completed", logger)
			case Failure(e) => Display.error(s"$name Test failed", logger, e)
		}
	}
}

// ---------------------------------------  EOF ----------------------------------------------
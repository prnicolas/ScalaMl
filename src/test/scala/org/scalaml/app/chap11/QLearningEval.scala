/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.app.chap11

import org.scalaml.reinforcement.qlearning.{QLearning, QLState, QLConfig, QLLabel}
import org.scalaml.plots.{ScatterPlot, LinePlot, LightPlotTheme}
import org.scalaml.workflow.data.DataSource
import org.scalaml.core.XTSeries
import org.scalaml.trading.YahooFinancials
import YahooFinancials._
import org.scalaml.core.Types.ScalaMl.DblVector



	 
object QLearningEval {
    			/*
    			 * Initial data set extracted from stock CSV files
    			 */
    final val alpha = 0.6
	final val gamma = 0.5
	final val maxIters = 150
			
	final val path = "resources/data/chap11/"
	final val etfs = Array[String]("TLT", "ICF", "SPY", "VWO", "USO", "IWC")

	final val states = Array[QLState[Double]](
	   QLState[Double](0, Array[Double](0.2, 0.2, 0.2, 0.0, 0.2, 0.0)),
	   QLState[Double](1, Array[Double](0.1, 0.1, 0.1, 0.1, 0.3, 0.3)),
	   QLState[Double](2, Array[Double](0.3, 0.3, 0.1, 0.1, 0.1, 0.1)),
	   QLState[Double](3, Array[Double](0.1, 0.2, 0.2, 0.2, 0.2, 0.1)),
	   QLState[Double](4, Array[Double](0.5, 0.1, 0.1, 0.1, 0.1, 0.1)),
	   QLState[Double](5, Array[Double](0.2, 0.3, 0.0, 0.2, 0.2, 0.1)),
	   QLState[Double](6, Array[Double](0.0, 0.0, 0.3, 0.2, 0.2, 0.2)),
	   QLState[Double](7, Array[Double](0.3, 0.0, 0.0, 0.2, 0.1, 0.4)),
	   QLState[Double](8, Array[Double](0.4, 0.1, 0.1, 0.0, 0.2, 0.2))
	)
	
	
    def run: Unit = {	
    	println("Evaluation Q-Learning algorithm")
    		// configuration of QLearning algorithm
	  	val qConfig = QLConfig(alpha, gamma, maxIters)
	  	
	  		// extract the input values for each symbol (stock time series)
	  	val values = etfs.map( etf =>
	  		DataSource(path + etf + ".csv", false, true, 1) |> YahooFinancials.adjClose )
	  		
	  		// If all values have been retrieved
	    values find( _ != None) match {
		   case Some(input) => {
		  	  val labels = List[(DblVector, QLState[Double])]()
		  	  
		  			  // start collecting the training labels
		  	  Range(40, 240).foreach( n => {
			     val goal = states.zipWithIndex
			                       .maxBy(p => p._1.distribution.reduceLeft( (s, x) => s + x + input.get(n)))._1
			     (input.get, goal) :: labels
		  	  })  
		  	    	
		  	  val qLabels = QLLabel[Double](labels)
		  	  val qLearning = QLearning[Double](qConfig, qLabels, states.size)
		   }
		   case None => Console.println("Failed to extract all input data")
	   }
    }
}


// ------------------------------------  EOF ----------------------------------
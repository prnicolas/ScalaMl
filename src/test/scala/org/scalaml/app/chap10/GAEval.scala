/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95
 */
package org.scalaml.app.chap10

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.{TradingStrategy, Signal, LESS_THAN, GREATER_THAN, EQUAL, NONE}
import org.scalaml.ga.{Operator, GASolver, GAConfig, Population, Chromosome, Discretization}
import scala.collection.mutable.ArrayBuffer
import org.scalaml.core.types.ScalaMl.{DblVector, DblMatrix}
import org.scalaml.core.XTSeries
import org.scalaml.trading.YahooFinancials
import scala.collection.mutable.HashMap
import org.scalaml.util.Display
import org.scalaml.trading.{SOperator, Signal, StrategyFactory}
import org.scalaml.ga.Gene
import Chromosome._
import org.apache.log4j.Logger
import org.scalaml.ga.GeneticIndices
import scala.util.{Try, Success, Failure}


/**
 *  @author Patrick Nicolas
 *  @date Aug 10, 2014
 *  @project Book
 */
object GAEval  {
   import org.scalaml.trading.YahooFinancials._
   final val path = "resources/data/chap10/GS.csv"
   final val XOVER = 0.8
   final val MU = 0.4
   final val MAX_CYCLES = 400
	   
   private val logger = Logger.getLogger("GAEval")

   
   def run: Int = { 
  	 Try {
  	    val src = DataSource(path, false, true, 1)
  	    		// Extract relative variation of price between two consecutive trading sessions
		val price = src |> YahooFinancials.adjClose
		val deltaPrice = price.drop(1).zip(price.take(price.size-1)).map(p => (1.0 - p._2/p._1))
				// Extract relative variation of volume between two consecutive trading sessions
		val volume = src |> YahooFinancials.volume
		val deltaVolume: DblVector = volume.drop(1).zip(volume.take(volume.size-1)).map(p => (p._2/p._1 - 1.0))
				// extract relative variation of volatility between two consecutive trading sessions
  	    val volatility = src |> YahooFinancials.relVolatility
	    val deltaVolatility = volatility.drop(1).zip(volatility.take(volatility.size-1)).map(p => (p._2/p._1 - 1.0))
	    		// Relative volatility within the session
        val relVolatility = src |> YahooFinancials.volatility
		       // Relative difference between close and open price
		val relCloseOpen = src |> YahooFinancials.relCloseOpen

		Display.show(deltaPrice, logger)
	    implicit val digital = Discretization((x: Double) =>(x*1024).toInt, (n: Int) => n/1024.0) 
	   
	    val NUM_SIGNALS_PER_STRATEGY = 3
	    val factory = new StrategyFactory(NUM_SIGNALS_PER_STRATEGY)
	    factory +=  ("Delta_volume", 1.1, GREATER_THAN, deltaVolume, deltaPrice)
	    factory +=  ("Rel_volatility", 1.3, GREATER_THAN, relVolatility.drop(1), deltaPrice)
	    factory +=  ("Rel_close-Open", 0.8, LESS_THAN, relCloseOpen.drop(1), deltaPrice)
	    factory +=  ("Delta_volatility", 0.9, GREATER_THAN, deltaVolatility, deltaPrice)
	
	    val population = Population[Signal]((factory.strategies.size <<4), factory.strategies)
	         // compute the score for the entire time series
	    population.display(s"Initial population: $population\n")
	   
	    val CUTOFF_SLOPE = -0.003
	    val CUTOFF_INTERCEPT = 1.003
	    val softLimit = (n: Int) => CUTOFF_SLOPE*n + CUTOFF_INTERCEPT
	    val config = GAConfig(XOVER, MU, MAX_CYCLES, softLimit )
	   
	    val scoring = (chr: Chromosome[Signal]) =>  {
	  	  val signals: List[Gene] = chr.code
		  chr.unfitness = signals.foldLeft(0.0)((sum, s) => sum + s.score)
	    }
	    val gaSolver = GASolver[Signal](config, scoring)
	   
	    val best = gaSolver |> population
	    best.fittest(2)
	        .getOrElse(ArrayBuffer.empty)
	        .foreach(ch => Display.show(s"Best strategy: ${ch.show}", logger))
	  	Display.show("GAEval.run completed", logger)
     } match {
    	case Success(n) => n
  	    case Failure(e) => Display.error("GAEval.run completed", logger, e)
     }
   }
}




object GAEvalApp extends App {
	GAEval.run
}

// ----------------  EOF ------------------------------------
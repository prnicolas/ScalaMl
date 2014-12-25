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
package org.scalaml.app.chap10



import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.{TradingStrategy, Signal, YahooFinancials, StrategyFactory}
import org.scalaml.trading.operator.{LESS_THAN, GREATER_THAN, EQUAL, NONE, SOperator}
import org.scalaml.ga.{Operator, GASolver, GAConfig, Population, Chromosome, Discretization, GeneticIndices, Gene}
import org.scalaml.core.XTSeries
import org.scalaml.util.{FormatUtils, DisplayUtils}
import org.scalaml.app.Eval
import org.scalaml.core.Types.ScalaMl.DblVector
		/**
		 * <p><b>Purpose</b>: Evaluate the convergence a genetic algorithm optimizer for extract 
		 * the best trading strategy for predicting the price movement of a stock.</p>
		 * 
		 * @see org.scalaml.gaGene 
		 * @see org.scalaml.trading.TradingStrategy
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm / GA for trading strategies 
		 * / Test case
		 */
object GAEval extends Eval {
	import scala.collection.mutable.ArrayBuffer
	import scala.util.{Try, Success, Failure}
	import org.apache.log4j.Logger
	import YahooFinancials._, Chromosome._
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "GAEval"
      
	private val path = "resources/data/chap10/GS.csv"
	private val XOVER = 0.8					// Probability or ratio for cross-over
	private val MU = 0.4					// Probability or ratio for mutation
	private val MAX_CYCLES = 400			// Maximum number of iterations during the optimization
	private val CUTOFF_SLOPE = -0.003		// Slope for the linear soft limit
	private val CUTOFF_INTERCEPT = 1.003	// Intercept value for the linear soft limit
	private val R = 1024  					// discretization ratio for conversion Int <-> Double
	private val NFITS = 2					// Number of fittest chromosomes to consider as solution candidates
   
	private val softLimit = (n: Int) => CUTOFF_SLOPE*n + CUTOFF_INTERCEPT	   
	private val NUM_SIGNALS_PER_STRATEGY = 3 // Number of trading signals per trading strategy 
																						// (= number of genes in a chromosome)
   	    
		// Default data conversion
	implicit val digitize = new Discretization(R)
   

		// Define the scoring function for the chromosomes (i.e. Trading strategies)
		// as the sum of the score of the genes (i.e. trading signals) in this chromosome (i.e strategy).
	val scoring = (chr: Chromosome[Signal]) =>  {
		val signals: List[Gene] = chr.code
		chr.unfitness = signals.foldLeft(0.0)((sum, s) => sum + s.score)
	}

		/** <p>Execution of the scalatest for <b>GASolver</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = { 
		DisplayUtils.show(s"$header Evaluation genetic algorithm", logger)
  	 
		Try {
			val strategies = createStrategies
			val population = Population[Signal]((strategies.size <<4), strategies)
	        
			population.symbolic(s"Initial population: $population\n")
	   
			val config = GAConfig(XOVER, MU, MAX_CYCLES, softLimit)
			val gaSolver = GASolver[Signal](config, scoring)
	   
			val best = gaSolver |> population
			best.fittest(2)
				.getOrElse(ArrayBuffer.empty)
				.foreach(ch => DisplayUtils.show(s"$name Best strategy: ${ch.symbolic("->")}", logger))
	        
			DisplayUtils.show(s"$name run completed", logger)
		} 
		match {
			case Success(n) => n
			case Failure(e) => failureHandler(e)
		}
	}
   
	
	private def createStrategies: Pool[Signal] = {
		val src = DataSource(path, false, true, 1)
  	    
		// Extract relative variation of price between two consecutive trading sessions
		val price = src |> YahooFinancials.adjClose
		val deltaPrice: DblVector = price.drop(1)
								.zip(price.dropRight(1))
								.map(p => (1.0 - p._2/p._1))
		
				// Extract relative variation of volume between two consecutive trading sessions
		val volume = src |> YahooFinancials.volume
		val deltaVolume: DblVector = volume.drop(1)
											.zip(volume.dropRight(1))
											.map(p => (p._2/p._1 - 1.0))
		                                   
				// extract relative variation of volatility between two consecutive trading sessions
		val volatility = src |> YahooFinancials.relVolatility
		val deltaVolatility = volatility.drop(1)
										.zip(volatility.dropRight(1))
										.map(p => (p._2/p._1 - 1.0))
	    
		// Relative volatility within the session
		val relVolatility = src |> YahooFinancials.volatility
		       
		// Relative difference between close and open price
		val relCloseOpen = src |> YahooFinancials.relCloseOpen

		DisplayUtils.show(s"$name GS Stock price variation", logger)
		DisplayUtils.show(s"${FormatUtils.format(deltaPrice, "", FormatUtils.ShortFormat)}", logger)
		
			// Generate the trading strategies as a unique combinations of 
			// NUM_SIGNALS_PER_STRATEGY trading signals (genes).
		val factory = new StrategyFactory(NUM_SIGNALS_PER_STRATEGY)
		factory +=  ("Delta_volume", 1.1, GREATER_THAN, deltaVolume, deltaPrice)
		factory +=  ("Rel_volatility", 1.3, GREATER_THAN, relVolatility.drop(1), deltaPrice)
		factory +=  ("Rel_close-Open", 0.8, LESS_THAN, relCloseOpen.drop(1), deltaPrice)
		factory +=  ("Delta_volatility", 0.9, GREATER_THAN, deltaVolatility, deltaPrice)
	    
		factory.strategies
   }
}

// ----------------  EOF ------------------------------------
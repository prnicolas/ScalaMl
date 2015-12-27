/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99
 */
package org.scalaml.app.chap10

import scala.collection.mutable.ArrayBuffer
import scala.util.{Try, Success, Failure}
	
import org.apache.log4j.Logger

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.{TradingStrategy, Signal, YahooFinancials, StrategyFactory}
import org.scalaml.trading.operator.{LESS_THAN, GREATER_THAN, EQUAL, NONE, SOperator}
import org.scalaml.stats.XTSeries
import org.scalaml.util.{FormatUtils, DisplayUtils,  LoggingUtils}
import org.scalaml.app.Eval
import org.scalaml.core.Types.emptyString
import org.scalaml.core.Types.ScalaMl.{DblArray, DblVector, DblPair}
import org.scalaml.ga._
import YahooFinancials._, Chromosome._, LoggingUtils._, FormatUtils._, Gene._, XTSeries._
	
		/**
		 * '''Purpose''': Evaluate the convergence a genetic algorithm optimizer for extract 
		 * the best trading strategy for predicting the price movement of a stock.
		 * 
		 * @see org.scalaml.ga.{Gene, Chromosome} 
		 * @see org.scalaml.trading.TradingStrategy
		 * @author Patrick Nicolas
		 * @see Scala for Machine Learning Chapter 10 Genetic Algorithm / GA for trading strategies 
		 * / Test case
		 */
object GAEval extends Eval {
	import scala.language.postfixOps


		/**
		 * Name of the evaluation 
		 */
	val name: String = "GAEval"
	  
      
	private val path = "resources/data/chap10/GS.csv"
	private val XOVER = 0.8					// Probability or ratio for cross-over
	private val MU = 0.4					// Probability or ratio for mutation
	private val MAX_CYCLES = 1000			// Maximum number of iterations during the optimization
	private val CUTOFF_SLOPE = -0.001		// Slope for the linear soft limit
	private val CUTOFF_INTERCEPT = 1.003	// Intercept value for the linear soft limit
	private val R = 1024  					// Quantization ratio for conversion Int <-> Double
	private val NFITS = 2					// Number of fittest chromosomes to consider as solution candidates
   
	private val softLimit = (n: Int) => CUTOFF_SLOPE*n + CUTOFF_INTERCEPT	   
	private val NUM_SIGNALS_PER_STRATEGY = 3 // Number of trading signals per trading strategy 
																						// (= number of genes in a chromosome)
 
		// Default data conversion
	implicit val digitize = new Quantization(R)
	implicit val encoding = defaultEncoding


	val relative = (xy: DblPair) => xy._2/xy._1 -1.0
	val invRelative = (xy: DblPair) => xy._2/xy._1 -1.0
		/**
		 * Define the scoring function for the chromosomes (i.e. Trading strategies)
		 * as the sum of the score of the genes (i.e. trading signals) in this chromosome (i.e strategy).
		 */
	val scoring = (chr: Chromosome[Signal]) =>  {
		val signals: List[Gene] = chr.code
		chr.cost = Math.log(signals.map(_.score).sum + 1.0)
	}
	
		/**
		 * Monitoring function for the GA execution
		 */
	private val averageCost = new ArrayBuffer[Double]
	private val tracker = (p: Population[Signal]) => averageCost.append(p.averageCost)


		/** Execution of the scalatest for '''GASolver''' class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate
		 * 
		 * Exceptions thrown during the execution of the tests are caught by the wrapper or handler
		 * test method in Eval trait defined as follows:
		 * {{{
		 *    def test(args: Array[String]) =
		 *      Try(run(args)) match {
		 *        case Success(n) => ...
		 *        case Failure(e) => ...
		 * }}}
		 * The tests can be executed through ''sbt run'' or individually by calling 
		 * ''TestName.test(args)'' (i.e. DKalmanEval.test(Array[String]("IBM") )
		 * @param args array of arguments used in the test
		 */
	override def run(args: Array[String]): Int = { 
		show(s"$header Evaluation genetic algorithm")
  	 

				// Create trading strategies
		createStrategies.map(strategies => {
			show(s"\n${strategies.mkString("\n")}")
			
				// Initialize the population with a upper bound of 16 times 
				// the initial number of strategies
			val initial = Population[Signal](strategies.size << 4, strategies)

			show(s"${initial.symbolic}")

				// Configure, instantiates the GA solver for trading signals
			val config = GAConfig(XOVER, MU, MAX_CYCLES, softLimit)
			val solver = GASolver[Signal](config, scoring, Some(tracker))
			
	
				// Extract the best population and the fittest chromosomes = trading strategies
				// from this final population.
			(solver |> initial).map(_.fittest.map(_.symbolic).getOrElse("NA")) match {
				case Success(results) =>
					display(averageCost.toArray)
					show(results)

				case Failure(e) => error("training ", e)
			}
		}).getOrElse(error("GAEval failed"))
	}

	private def display(data: DblArray): Unit = {
	  import org.scalaml.plots.{Legend, LinePlot, LightPlotTheme}
	  
		val labels = new Legend(name, "Genetic algorithm convergence", "Recursions", "Average cost")
		LinePlot.display(data, labels, new LightPlotTheme)
	}
	

		/*
		 * Create Trading strategies by loading price data from Yahoo financial tables.
		 */
	private def createStrategies: Try[Pool[Signal]] = {
		val src = DataSource(path, false, true, 1)
		
		for {
			price <- src.get(adjClose)
			dPrice <- delta(price, -1.0) 
			volume <- src.get(volume)
			dVolume <- delta(volume, 1.0)
			volatility <- src.get(volatility)
			dVolatility <- delta(volatility, 1.0)
			vPrice <- src.get(vPrice)
		} 
		yield {
			show(s"""GS Stock price variation
				| ${format(dPrice, "GS stock price daily variation", SHORT)}""".stripMargin )
			
			val factory = new StrategyFactory(NUM_SIGNALS_PER_STRATEGY)
	
			val avWeights = dPrice.sum/dPrice.size
			val weights = Vector.fill(dPrice.size)(avWeights)
			factory +=  ("dVolume", 1.1, GREATER_THAN, dVolume, weights)
			factory +=  ("volatility", 1.3, GREATER_THAN, volatility.drop(1), weights)
			factory +=  ("change", 0.8, LESS_THAN, vPrice.drop(1), weights)
			factory +=  ("dVolatility", 0.9, GREATER_THAN, dVolatility, weights)

			factory.strategies
		}
	}
	
	private def delta(series: DblVector, a: Double): Try[DblVector] = 
		Try(zipWithShift(series, 1).map{ case(x, y) => a*(y/x - 1.0)})
}


// ----------------  EOF ------------------------------------
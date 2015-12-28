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
package org.scalaml.app.chap11

import scala.util.{Try, Success, Failure}

import org.scalaml.reinforcement.qlearning.{QLearning, QLInput, QLConfig}
import org.scalaml.workflow.data.DataSource
import org.scalaml.stats.XTSeries._
import org.scalaml.core.Types.ScalaMl.{DblArray, DblPair, DblVector}
import org.scalaml.trading.{OptionModel, YahooFinancials}
import org.scalaml.util.DisplayUtils
import org.scalaml.app.Eval
import org.scalaml.reinforcement.qlearning.QLModel
import QLConfig._


		 /**
		 * '''Purpose''': Singleton to Q-Learning algorithm to extract best trading policies.
		 * 
		 * The test is performed with two different type of goals:
		 * 
		 * - State which is the destination of the action with the highest reward
		 * - A random state
		 * 
		 * The probability value in the Input is used to model the noise in defining states.
		 * 
		 * @author Patrick Nicolas 
		 * @see Scala for Machine Learning Chapter 11 ''Reinforcement learning'' / Q-Learning
		 */
object QLearningEval extends Eval {
	import scala.collection.mutable.{ArrayBuffer, ListBuffer}
	import org.apache.log4j.Logger
	import YahooFinancials._

		/**
		 * Name of the evaluation 
		 */
	val name: String = "QLearningEval"

		// Files containing the historical prices for the stock and option
	private val STOCK_PRICES = "resources/data/chap11/IBM.csv"
	private val OPTION_PRICES = "resources/data/chap11/IBM_O.csv"
	  
		// Run configuration parameters
	private val STRIKE_PRICE = 190.0			// Option strike price
	private val MIN_TIME_EXPIRATION = 6		// Minimum expiration time for the option recorded
	private val QUANTIZATION_STEP = 32			// Quantization step (Double => Int)
	private val ALPHA = 0.2								// Learning rate
	private val DISCOUNT = 0.6						// Discount rate used in the Q-Value update equation
	private val MAX_EPISODE_LEN = 128			// Maximum number of iteration for an episode
	private val NUM_EPISODES = 20					// Number of episodes used for training.
	
	private val NUM_NEIGHBHBOR_STATES = 3	// Number of states accessible from any other state
				

	
		/** 
		 * Execution of the scalatest for '''QLearning''' class.
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
	override protected def run(args: Array[String]): Int = { 
		show(s"$header Q-learning")
		run("Maximum reward", QUANTIZATION_STEP, ALPHA, DISCOUNT)
		run("Random", QUANTIZATION_STEP, ALPHA, DISCOUNT)
	}
	
	
	def run(rewardType: String, quantizeR: Int, alpha: Double, gamma: Double): Int = {		 
		show(s"""Q-learning type $rewardType goal quantization rate $quantizeR
				| learning rate $alpha and discount rate $gamma""".stripMargin)

		
			// Retrieve the historical stock price, IBM, used as underlying security to the option
		val src = DataSource(STOCK_PRICES, false, false, 1)
	
			// Extract the historical price of the option and create a model
			// The for-comprehensive loop is used to process the sequence of 
			// options as returned values
		val model = for {
			option <- Try(createOptionModel(src, quantizeR))
			oPrices <- DataSource(OPTION_PRICES, false, false, 1).extract
			_model <- createModel(option, oPrices, alpha, gamma)
		} yield _model
		
			// Display the distribution of values in the model and
			// display the estimates Q-value for the best policy on a Scatter plot
		model.map(m => {
			if( rewardType != "Random")
				display(m.bestPolicy.EQ, m.toString, s"$rewardType with quantization order $quantizeR" )
			1
		}).get
	}
	
	private def createOptionModel(src: DataSource, quantizeR: Int): OptionModel = 
	  new OptionModel("IBM",STRIKE_PRICE, src, MIN_TIME_EXPIRATION, quantizeR) 
	  
	
		/*
		 * Create a model for the profit and loss on an option given 
		 * the underlying security. The profit and loss is adjusted to 
		 * produce positive values.
		 */
	private def createModel(
			ibmOption: OptionModel, 
			oPrice: Seq[Double],
			alpha: Double, 
			gamma: Double): Try[QLModel] = {
		
		import scala.util.Random
		
			// quantize the value of the option oPrice
		val qPriceMap = ibmOption.quantize(oPrice.toArray)
		val numStates = qPriceMap.size
		
			/**
			 * Constraining method to limit the number of actions available
			 * to any given state. This simple implementation identifies 
			 * the neigbhoring state within a predefined radius
			 */
		val neighbors = (n: Int) => {
					// Compute the list of all the states within a radius
					// of this states.
			def getProximity(idx: Int, radius: Int): List[Int] = {
				val idx_max = if(idx + radius >= numStates) numStates-1 else idx+ radius
				val idx_min = if(idx < radius) 0 else idx - radius
				Range(idx_min, idx_max+1).filter( _ != idx)./:(List[Int]())((xs, n) => n :: xs)
			}
			getProximity(n, NUM_NEIGHBHBOR_STATES)
		}
		
		
			// Compute the minimum value for the profit, loss so the maximum
			// loss is converted to a null profit
		val qPrice: DblVector = qPriceMap.values.toVector
		val profit: DblVector = normalize(zipWithShift(qPrice, 1).map{ case(x, y) => y -x }).get
		val maxProfitIndex = profit.zipWithIndex.maxBy(_._1)._2
	
		val reward = (x: Double, y: Double) => Math.exp(30.0*(y-x))
		val probabilities = (x: Double, y: Double) => if(y < 0.3*x) 0.0 else 1.0
						
		show(s"Goal state index: $maxProfitIndex")
		
			// Create a Q-learning algorithm
		Try {
			if( !QLearning.validateConstraints(profit.size, neighbors) ) 
				throw new IllegalStateException("QLearningEval Incorrect states transition constraint")
			
			val instances = qPriceMap.keySet.toSeq.drop(1)
			val config = QLConfig(alpha, gamma, MAX_EPISODE_LEN, NUM_EPISODES)
			val qLearning = QLearning[Array[Int]](config, 
					Array[Int](maxProfitIndex), 
					profit, 
					reward, 
					probabilities, 
					instances,
					Some(neighbors))
				
			val coverage = qLearning.getModel.get.coverage
			val numTransitions = numStates*(numStates-1)
			show(s"Coverage $coverage for $numStates states and $numTransitions transitions")
					
			val profile = qLearning.dump
			show(s"Execution profile\n$profile")
			
			display(qLearning)
			qLearning
		}.
		map( _.getModel.get )
	}
	
	
	private def display(qLearning: QLearning[Array[Int]]): Unit = {
		import org.scalaml.plots.Legend
		
		val labels = Legend(
			name, "Q-learning Q-values per state", "Q-values", "States"
		)
		qLearning.display(QLearning.QVALUE_COUNTER, labels)
	}
			/*
			 * Display the estimated Q-value = value * probability
			 * in a Scatter plot.
			 */
	private def display(eq: Vector[DblPair], results: String, params: String): Unit = {
		import org.scalaml.plots.{ScatterPlot, BlackPlotTheme, Legend}
		
		val labels = Legend(
			name, s"Q-learning config: $params", "States", "States"
		)
		ScatterPlot.display(eq, labels, new BlackPlotTheme)
	}
}


// ------------------------------------  EOF ----------------------------------
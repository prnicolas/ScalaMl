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
package org.scalaml.app.chap11

import scala.util.{Try, Success, Failure}

import org.scalaml.reinforcement.qlearning.{QLearning, QLInput, QLConfig}
import org.scalaml.workflow.data.DataSource
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl.{DblVector, XYTSeries}
import org.scalaml.trading.{OptionModel, YahooFinancials}
import org.scalaml.util.DisplayUtils
import org.scalaml.app.Eval
import org.scalaml.reinforcement.qlearning.QLModel

		 /**
		 * <p><b>Purpose</b>: Singleton to Q-Learning algorithm to extract
		 * best trading policies.<br>
		 * The test is performed with two different type of goals:
		 * <ul>
		 * <li>State which is the destination of the action with the highest reward</li>
		 * <li>A random state</li>
		 * </ul>
		 * The probability value in the Input is used to model the noise in defining states.</p>
		 * 
		 * @author Patrick Nicolas 
		 * @note Scala for Machine Learning Chapter 11 Reinforcement learning / Q-Learning
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
	private val FUNCTION_APPROX_STEP = 4	// Discretization step (Double => Int)
	private val ALPHA = 0.3								// Learning rate
	private val DISCOUNT = 0.6						// Discount rate used in the Q-Value update equation
	private val MAX_EPISODE_LEN = 35			// Maximum number of iteration for an episode
	private val NUM_EPISODES = 80					// Number of episodes used for training.
	
	private val NUM_NEIGHBHBOR_STATES = 3	// Number of states accessible from any other state
		/** 
		 * <p>Execution of the scalatest for <b>QLearning</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = { 
		val goalStr = if(args.size > 0) args(0) else "random"
		val numNeighborStates: Int = if(args.size > 1) args(1).toInt else NUM_NEIGHBHBOR_STATES
		val functionApproxStep: Int = if(args.size > 2) args(2).toInt else FUNCTION_APPROX_STEP
						  		
			// List the neighbors of this state at index idx within
			//a radius RADIUS, allowed as the next states
		val getNeighborsStates = (idx: Int, numStates: Int) => {
				// Compute the list of all the states within a radius
				// of this states.
			def getProximity(idx: Int, radius: Int): List[Int] = {
				val idx_max = if(idx + radius >= numStates) numStates-1 else idx+ radius
				val idx_min = if(idx < radius) 0 else idx - radius
				Range(idx_min, idx_max+1).filter( _ != idx)
										.foldLeft(List[Int]())((xs, n) => n :: xs)
			}
			getProximity(idx, numNeighborStates).toList
		}
		val params = s" with $numNeighborStates neighbors and $functionApproxStep approx steps"
		val desc = s"$header Q-learning with $goalStr goal "
		DisplayUtils.show(s"$desc$params", logger)
		
			// Retrieve the historical stock price, IBM, used as underlying security to the option
		val src = DataSource(STOCK_PRICES, false, false, 1)
		
			// Create an option 
		val ibmOption = new OptionModel("IBM", STRIKE_PRICE, src, MIN_TIME_EXPIRATION, functionApproxStep)

			// Extract the historical price of the option and create a model
			// The for-comprehensive loop is used to process the sequence of 
			// options as returned values
		val model = for {
			v <- DataSource(OPTION_PRICES, false, false, 1).extract
			_model <- createModel(ibmOption, v, goalStr, getNeighborsStates)
		} yield _model
		
			// Display the distribution of values in the model and
			// display the estimates Q-value for the best policy on a Scatter plot
		model.map(m => {
			DisplayUtils.show(s"$name ${m.toString}", logger)
			if( goalStr != "random")
				display(m.bestPolicy.EQ, params)
			1
		})
		.getOrElse(DisplayUtils.show(s"$name Failed to create a model: Poor coverage", logger))
	}
	
		/*
		 * Create a model for the profit and loss on an option given 
		 * the underlying security. The profit and loss is adjusted to 
		 * produce positive values.
		 */
	private def createModel(
			ibmOption: OptionModel, 
			oPrice: DblVector,
			goalStr: String,
			getNeighborsStates: (Int,Int) => List[Int]): Option[QLModel[Array[Int]]] = {
		
		import scala.util.Random
			// Discretize the value of the option oPrice
		val fMap = ibmOption.approximate(oPrice)
		val numStates = fMap.size
		
			// Compute the minimum value for the profit, loss so the maximum
			// loss is converted to a null profit
		val _min = fMap.values.zip(fMap.values).map(x => x._1 - x._2).min
		val profits = fMap.values.zipWithIndex
		
			// Create the QLInput matrix with a random probability.
			// The random probability is used to model the noise in defining states
		val input = new ArrayBuffer[QLInput]
		profits.foreach(v1 => 
			profits.foreach(v2 => 
				input.append(new QLInput(v1._2, v2._2, v1._1 - v2._1 + _min, 0.2*Random.nextDouble)))
		)
		
			// Select the goal state as the destination of the state with the
			// highest reward or as a randomly selected state.
		val goal = if(goalStr == "maxReward") input.maxBy( _.reward).to 
						else Random.nextInt(numStates)
						
		DisplayUtils.show(s"$name Goal state: ${goal.toString}", logger)
		
			// Create a Q-learning algorithm
		Try {
			val config = QLConfig(ALPHA, DISCOUNT, MAX_EPISODE_LEN, NUM_EPISODES, getNeighborsStates)
			QLearning[Array[Int]](config, numStates, goal, input.toArray, fMap.keySet)
		}
		match {
			// Extract the model if the training completes.
			case Success(qLearning) => qLearning.getModel
			case Failure(e) => {failureHandler(e); None }
		}
	}
    
			/*
			 * Display the estimated Q-value = value * probability
			 * in a Scatter plot.
			 */
	private def display(eq: XYTSeries, params: String): Unit = {
		import org.scalaml.plots.{ScatterPlot, BlackPlotTheme}
		val labels = List[String](
			name, s"Q-learning best policy Q-values $params", "States", "States"
		)
		ScatterPlot.display(eq, labels, new BlackPlotTheme)
	}
}

// ------------------------------------  EOF ----------------------------------
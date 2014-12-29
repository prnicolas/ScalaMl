/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.reinforcement.qlearning

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

import org.scalaml.util.DisplayUtils
import org.scalaml.core.Matrix
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.Design.{Config, PipeOperator}
import QLearning._


		/**
		 * </p>Parameterized class that defines the configuration parameters for the Q-Learning.</p>
		 * @constructor Create a configuration for the Q-learning algorithm. 
		 * @throws IllegalArgumentException if alpha, gamma or maximum iteration are out of range
		 * @param alpha Learning rate for the Q-Learning algorithm.
		 * @param gamma  Discount rate for the Q-Learning algorithm.
		 * @param episodeLength Maximum number of states visited per episode.
		 * @param numEpisodes  Number of episodes used during training.
		 * @param minCoverage Minimum coverage allowed during the training of the Q-learning model. 
		 * The coverage is the percentage of episodes for which the goal state is reached.
		 * @param neighbors  Function that list the available states neighbors to the current state 
		 * during execution. 
		 * 
		 * @author Patrick Nicolas
		 * @since January 19, 2014
		 * @note Scala for Machine Learning Chapter 11 Reinforcement learning / Q-learning
		 */
class QLConfig(
		val alpha: Double, 
		val gamma: Double, 
		val episodeLength: Int, 
		val numEpisodes: Int, 
		val minCoverage: Double, 
		val neighbors: (Int, Int) =>List[Int]) extends Config {
	import QLConfig._
    
	check(alpha, gamma, episodeLength, numEpisodes, minCoverage, neighbors)
}


		/**
		 * Companion object for the configuration of the Q-learning algorithm. This singleton defines
		 * the constructor for the QLConfig class and validates its parameters.
		 * @author Patrick Nicolas
		 * @since January 19, 2014
		 * @note Scala for Machine Learning Chapter 11 Reinforcement learning/Q-learning
		 */
object QLConfig {
	private val NO_MIN_COVERAGE = 0.0
		/**
		 * Default constructor for configuration of Q-learning
		 * @param alpha Learning rate for the Q-Learning algorithm.
		 * @param gamma  Discount rate for the Q-Learning algorithm.
		 * @param episodeLength Maximum number of states visited per episode.
		 * @param numEpisodes  Number of episodes used during training.
		 * @param minCoverage Minimum coverage allowed during the training of the Q-learning model. 
		 * The coverage is the percentage of episodes for which the goal state is reached.
		 * @param neighbors  Function that list the available states neighbors to the current state 
		 * during execution. 
		 */
	def apply(
			alpha: Double, 
			gamma: Double, 
			episodeLength: Int, 
			numEpisodes: Int, 
			minCoverage: Double, 
			neighbors: (Int, Int) =>List[Int]): QLConfig = 
		new QLConfig(alpha, gamma, episodeLength, numEpisodes, minCoverage,  neighbors)

		/**
		 * Constructor for configuration of Q-learning with no minimum coverage for the training phase
		 * @param alpha Learning rate for the Q-Learning algorithm.
		 * @param gamma  Discount rate for the Q-Learning algorithm.
		 * @param episodeLength Maximum number of states visited per episode.
		 * @param numEpisodes  Number of episodes used during training.
		 * @param neighbors  Function that list the available states neighbors to the current state 
		 * during execution. 
		 */
	def apply(
			alpha: Double, 
			gamma: Double, 
			episodeLength: Int, 
			numEpisodes: Int, 
			neighbors: (Int, Int) =>List[Int]): QLConfig = 
		new QLConfig(alpha, gamma, episodeLength, numEpisodes, NO_MIN_COVERAGE,  neighbors)

		
	private val MAX_EPISODES = 1000
	private val MAX_MIN_COVERAGE = 0.9

	private def check(
			alpha: Double, 
			gamma: Double, 
			episodeLength: Int, 
			numEpisodes: Int, 
			minCoverage: Double, 
			neighbors: (Int, Int) =>List[Int]): Unit = {
	  
		require(alpha > 0.0 && alpha < 1.0, 
				s"QLConfig.check Cannot initialize QLearning with incorrect alpha $alpha")
		require(gamma > 0.0 && gamma < 1.0, 
				s"QLConfig.check Cannot initialize QLearning with incorrect gamma $gamma")
		require(numEpisodes > 2 && numEpisodes < MAX_EPISODES, 
				s"QLConfig.check  Number of episodes $numEpisodes is out of range")
		require(minCoverage >= 0.0 && minCoverage < MAX_MIN_COVERAGE, 
				s"QLConfig.check  Minimum coverage $minCoverage is out of range")
	}
}


// ----------------------------  EOF --------------------------------------------------------------

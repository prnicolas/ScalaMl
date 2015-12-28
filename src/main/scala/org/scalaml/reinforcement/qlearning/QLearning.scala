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
package org.scalaml.reinforcement.qlearning
	
	// Scala standard library
import scala.collection._
import scala.collection.mutable.ArrayBuffer
import scala.util.{Random, Try, Success, Failure}
	// 3rd party library
import org.apache.log4j.Logger
	// ScalaMl classes
import org.scalaml.util.LoggingUtils._
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.ETransform
import org.scalaml.core.Design.{Config, Model}


		/**
		 * Define a model for the Q-learning algorithm as the tuple <optimum policy, 
		 * training epoch coverage>.
		 * @constructor Model created during training of Q-learning. 
		 * @param bestPolicy Best policy computed or estimated during training.
		 * @param coverage  Ratio of training trial or epochs that reach a predefined goal state.
		 * @see org.scalaml.core.Design.Model
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98 January 22, 2014
		 * @see Scala for Machine Learning Chap 11 ''Reinforcement learning'' /Q-learning
		 */
final class QLModel(val bestPolicy: QLPolicy, val coverage: Double) extends Model {
		/**
		 * Textual representation of the model for the Q-learning algorithm
		 */
	override def toString: String = 
			s"Optimal policy: ${bestPolicy.toString}\n with coverage: $coverage" 
}

		/**
		 * Generic parameterized class that implements the QLearning algorithm. The Q-Learning
		 * model is initialized and trained during the instantiation of the class so it can be 
		 * in the correct state for the run-time prediction. Therefore the class instances have only
		 * two states successfully trained and failed training.
		 * 
		 * The implementation does not assume that every '''episode''' (or '''epoch''' or training 
		 * '''cycle''') is successful. 
		 * At completion of the training, the ratio of labels over initial training set is computed.
		 * The client code is responsible to evaluate the quality of the model by testing the ratio
		 * gamma a threshold.
		 * {{{
		 * Q-Learning value action 
		 *   Q'(t) = Q(t) + alpha.[r(t+1) + gamma.max{Q(t+1)} - Q(t)
		 * }}}
		 * @constructor Create a Q-learning algorithm. [config] Configuration for Q-Learning algorithm. 
		 * @tparam T type of the object for which the state is used in the reinforcement learning
		 * @param config Configuration for the Q-learning algorithm
		 * @param qlSpace Initial search space of states
		 * @param qlPolicy Initial policy for the search
		 * 
		 * @author Patrick Nicolas
		 * @since January 22, 2014
		 * @see Scala for Machine Learning Chap 11 "Reinforcement learning" / Q-learning
		 */
final class QLearning[T](
		conf: QLConfig, 
		qlSpace: QLSpace[T], 
		qlPolicy: QLPolicy) 
			extends ETransform[QLConfig](conf) with Monitor[Double] {
	import QLearning._
	
  type U = QLState[T]
  type V = QLState[T]
	protected val logger = Logger.getLogger("QLearning")


		/**
		 * Model parameters for Q-learning of type QLModel that is defined as
		 * a list of policies and training coverage 
		 */
	private[this] val model: Option[QLModel] = train

		/**
		 * Access the input data used for training the Q-learning algorithm
		 */
	def getInput: Seq[QLInput] = qlPolicy.input
	  
		/**
		 * Prediction of next state using Q-Learning model. The model is automatically 
		 * created (or trained) during instantiation of the class. It overrides the
		 * data transformation method (PipeOperator) with a transformation of a state to a 
		 * predicted goal state.
		 * @throws MatchError if the model is undefined or the input state is undefined 
		 * @return PartialFunction of state of type QLState[T] as input to a predicted goal state of 
		 * type QLState[T]
		 */
	override def |> : PartialFunction[U, Try[V]] = {
		case st: U if(isModel) => 
			Try(if( st.isGoal) st else nextState(QLIndexedState[T](st, 0)).state)
	}

		/**
		 * Retrieve the model for Q-learning as an option
		 */
	@inline 
	final def getModel: Option[QLModel] = model
	
	@inline 
	final def isModel: Boolean = model != None
	  
	override def toString: String = qlPolicy.toString + qlSpace.toString

		/*
		 * Recursive computation of the next most rewarding state
		 */
	@scala.annotation.tailrec
	private def nextState(iSt: QLIndexedState[T]): QLIndexedState[T] =  {
			// Among all the states
		val states = qlSpace.nextStates(iSt.state)
		
		if( states.isEmpty || iSt.iter >= config.episodeLength) 
			iSt
			// Select the state with the most reward given 
			// the current policy _bestPolicy'
		else {
			val fromId = iSt.state.id
			val qState = states.maxBy(s => model.map(_.bestPolicy.EQ(fromId, s.id)).getOrElse(-1.0))
			nextState(QLIndexedState[T](qState, iSt.iter+1))
		}
	}

		/*
		 * Online raining method for the Q-learning algorithm
		 */
	private def train: Option[QLModel] = 	Try {
				// Apply the train method for each episode with
				// randomly generated initial states.
	
		val completions = Range(0, config.numEpisodes).map(epoch =>
			if(train(-1)) 1 else 0
		).sum
			
				// Compute the coverage as the number of episodes
				// for each the goal state was reached..
		completions.toDouble/config.numEpisodes
		
				// The model is effectively created if the minimum coverage is reached
	}.map( coverage => {
		if(coverage > config.minCoverage) Some(new QLModel(qlPolicy, coverage)) else None
	}).get

	
		/**
		 * The state space is traversed recursively..
		 */
	private def train(state0: Int): Boolean = {

			// Recursive local function
		@scala.annotation.tailrec
		def search(iSt: QLIndexedState[T]): QLIndexedState[T] = {	
			
				
				// Get all the states adjacent to st
			val states = qlSpace.nextStates(iSt.state)
			if( states.isEmpty || iSt.iter >= config.episodeLength) 
				QLIndexedState(iSt.state, -1)
				
			else {
				// select the most rewarding of the list of adjacent states
				val state = states.maxBy(s => qlPolicy.EQ(iSt.state.id, s.id))
					
				// If the next most rewarding state is a goal state, we are done..
				if( qlSpace.isGoal(state) ) 
					QLIndexedState(state, iSt.iter)

				
				// Otherwise recompute the policy value for the state transition
				// using the reward matrix QLPolicy.R
				else {
					val fromId = iSt.state.id
					val r = qlPolicy.R(fromId, state.id)   
					val q = qlPolicy.Q(fromId, state.id)

					// Apply the Q-learning updating formula
					val nq = q + config.alpha*(r + config.gamma*qlSpace.maxQ(state, qlPolicy) - q)
					count(QVALUE_COUNTER, nq)

					// Update the Q-Value for the policy then invoke the
					// the search method with the new state and incremented iterator.
					qlPolicy.setQ(fromId, state.id,  nq)
					search(QLIndexedState(state, iSt.iter+1))
				}
			}
		}	
		
			// Select a random state to initialize the search.
		val finalState = search(QLIndexedState(qlSpace.init(state0), 0))
		if( finalState.iter == -1) 
			false
		else
			qlSpace.isGoal(finalState.state)
	}
}


		/**
		 * Input to the Q-learning search space (QLSpace) and policy (QLPolicy).
		 * @constructor Create an action input to Q-learning
		 * @param from Identifier for the source state
		 * @param to Identifier for the target or destination state
		 * @param reward reward (credit or penalty) to transition from state with id '''from''' 
		 * to the state with id '''to'''
		 * @param prob Probability to transition from state '''from''' to state '''to'''
		 * @author Patrick Nicolas
		 * @since January 22, 2014
		 * @note Scala for Machine Learning Chap 11 Reinforcement learning/Q-learning
		 */
case class QLInput(
		val from: Int, 
		val to: Int, 
		val reward: Double = 1.0, 
		val prob: Double = 1.0)


		/**
		 * Companion object to the Q-Learning class used to define constructors 
		 * and validate their input parameters
		 * @author Patrick Nicolas
		 * @since 0.98 January 22, 2014
		 * @version 0.99
		 * @see Scala for Machine Learning Chap 11 Reinforcement learning Q-learning
		 */
object QLearning {  
	final val QVALUE_COUNTER = "Q-Value"
  
  
		/**
		 * Default constructor for Q-learning
		 * @tparam T type of the instance or object which state is managed by Q-learning algorithm
		 * @param config Configuration for the Q-learning algorithm
		 * @param qlSpace Initial search space of states
		 * @param qlPolicy Initial policy for the search
		 */
	def apply[T](config: QLConfig, qlSpace: QLSpace[T], qlPolicy: QLPolicy): QLearning[T] =
		new QLearning[T](config, qlSpace, qlPolicy)

		/** 
		 * Constructor of the Q-learning with a given configuration, array of id of goal states,
		 * input (or initial) states, the sequence of instance associated to each state and
		 * the optional contraints on actions available to any given state during training.
		 * @tparam T type of the instance or object which state is managed by Q-learning algorithm
		 * @param config Configuration for the Q-learning algorithm
		 * @param input Input to the search space as tuple of rewards and probability for each 
		 * action between states
		 * @param Instances Sequence of instances associated to the states managed by Q-learning
		 * @param constraints User provided function that computes the list of states available to a
		 * given state through an action.
		 * @throws IllegalArgumentException if one of the parameters is undefined or improperly initialized
		 */
	@throws(classOf[IllegalArgumentException])
	def apply[T](
			config: QLConfig, 
			goals: Array[Int], 
			input: => Seq[QLInput], 
			instances: Seq[T],
			constraints: Option[Int => List[Int]] = None): QLearning[T] = {
	  
	
		require( !input.isEmpty, "QLearning Cannot initialize with undefine input rewards")
		require( goals.length > 0, "QLearning Cannot initialize with undefined goals")
		require( !instances.isEmpty, "QLearning Cannot initialize with undefined features")
		
		new QLearning[T](
				config, 
				QLSpace[T](goals, instances, constraints), 
				new QLPolicy(input))
	}
	
			/** 
		 * Constructor of the Q-learning with a given configuration, array of id of goal states,
		 * input (or initial) states, the input data set, a reward function used in the
		 * creation of the rewards matrix, a probability function used in the creation of the
		 * probabilities matrix, the sequence of instance associated to each state and
		 * the optional contraints on actions available to any given state during training.
		 * 
		 * The ''input'' or initial states are computed from the data set, ''xt'' and the 
		 * ''reward'' and the ''probability'' functions are 
		 * 
		 * @tparam T type of the instance or object which state is managed by Q-learning algorithm
		 * @param config Configuration for the Q-learning algorithm
		 * @param xt Input date set
		 * @param reward Function that populates the rewards matrix
		 * @param probability Function that populates the probabilities matrix
		 * @param Instances Sequence of instances associated to the states managed by Q-learning
		 * @param constraints User provided function that computes the list of states available to a
		 * given state through an action.
		 * @throws IllegalArgumentException if one of the parameters is undefined or improperly initialized
		 */
	@throws(classOf[IllegalArgumentException])
	def apply[T](
			config: QLConfig, 
			goals: Array[Int],
			xt: DblVector,
			reward: (Double, Double) => Double,
			probability: (Double, Double) => Double,
			instances: Seq[T],
			constraints: Option[Int =>List[Int]]): QLearning[T] = {
	
		require( xt.size > 2, s"QLearning Cannot initialize with ${xt.size} states")
		require( goals.length > 0, "QLearning Cannot initialize with undefined goals")
		require( !instances.isEmpty, "QLearning Cannot initialize with undefined features")
		
		
		val r = Range(0, xt.size)
		val input = new ArrayBuffer[QLInput]
		r.foreach(i => 
			r.foreach(j => 
				input.append( QLInput(i, j, reward(xt(i), xt(j)), probability(xt(i), xt(j))))
			)
		)
		new QLearning[T](config, QLSpace[T](goals, instances, constraints), new QLPolicy(input))
	}
	
			/**
			 * Object method that validate the constraining function. This method 
			 * verifies that the constraints on the actions available to any given state
			 * generates non-empty list of next states.
			 * @param numStates Number of states for which the constraints function need 
			 * to be validated
			 * @param constraint Constraint function that need to be validated
			 * @return true if any given state except the goal has at least one next state
			 * false otherwise
			 */
	@throws(classOf[IllegalArgumentException])
	def validateConstraints(numStates: Int, constraint: Int => List[Int]): Boolean = {
		require(numStates > 1, s"QLearning validateConstraints found $numStates states should be >1")
	
		!Range(0, numStates).exists( constraint(_).isEmpty)
	}

		/** 
		 * Constructor of the Q-learning with a predefined number of states, index of the goal state,
		 * input (or initial) states and a features set.
		 * @param config Configuration for the Q-learning algorithm
		 * @param numStates Number of states in the model 
		 * @param goal Index of the goal state
		 * @param input Input to the search space
		 * @param features Features set as a set of variables of type Array[Int] generated through 
		 * quantization of signal (floating point values)
		 */
	def apply[T](
			config: QLConfig, 
			goal: Int, 
			input: => Seq[QLInput], 
			instances: Seq[T],
			constraints: Option[Int => List[Int]]): QLearning[T] = 
		apply[T](config, Array[Int](goal), input, instances, constraints)
}


// ----------------------------  EOF --------------------------------------------------------------

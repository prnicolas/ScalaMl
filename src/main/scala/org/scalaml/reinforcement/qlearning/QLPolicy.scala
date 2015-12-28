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
 * Version 0.99.1
 */
package org.scalaml.reinforcement.qlearning

import org.scalaml.core.Types.ScalaMl.DblPair


		/**
		 * Enumerator to define the type of a parameters used to update the policy during
		 * the training of Q-learning model.
		 */
object QLDataVar extends Enumeration {
	type QLDataVar = Value
	val REWARD, PROB, VALUE = Value
}


		/**
		 * Class that encapsulates the attributes of the policy in Q-learning algorithm
		 * @constructor Create a QLData record or instance with a given reward, probability and
		 * a Q-value that is computed and updated during training.
		 * @param reward  reward assigned during initialization
		 * @param probability probability (or hindrance) assigned during initialization
		 * @author Patrick Nicolas
		 * @since 0.98 January 25, 2014
		 * @version 0.98.2
		 * @see Scala for Machine Learning Chap 11 ''Reinforcement learning'' / Q-learning
		 */
final protected class QLData(val reward: Double, val probability: Double = 1.0) {
	import QLDataVar._
	
		/**
		 * Q-Value updated during training using the Q-learning formula
		 */
	var value: Double = 0.0
	
		/**
		 * Compute the overall value for an action using its reward, adjusted by its probability
		 * @return adjusted value
		 */
	@inline
	final def estimate: Double = value*probability
	
		/**
		 * Select the attribute of an element of Q-learning policy using its type
		 * @param varType of the attribute {REWARD, PROBABILITY, VALUE}
		 * @return value of this attribute
		 */
	final def value(varType: QLDataVar): Double = varType match {
		case REWARD => reward
		case PROB => probability
		case VALUE => value
	}
	
	override def toString: String =s"\nValue= $value Reward= $reward Probability= $probability"
}


		/**
		 * Class that defines the policy for a given set of input and a number of states.
		 * @constructor Create a policy as a model for Q-learning. 
		 * @throws IllegalArgumentException if the class parameters are undefined
		 * @param input Input to initialize the policy.
		 * @see org.scalaml.design.Model
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98 January 25, 2014
		 * @note Scala for Machine Learning Chap 11 Reinforcement learning/Q-learning
		 */
@throws(classOf[IllegalArgumentException])
final protected class QLPolicy(val input: Seq[QLInput]) {
	import QLDataVar._
	
	QLPolicy.check(input)

	
		/**
		 * Initialization of the policy using the input probabilities and rewards
		 */
	private[this] val qlData = input.map(qlIn => new QLData(qlIn.reward, qlIn.prob))
	private[this] val numStates = Math.sqrt(input.size).toInt


		/**
		 * Set the Q value for an action from state from to state to
		 * @param from Source state for the action for which the value is updated
		 * @param to destination state for the action for which the value is updated
		 * @throws IllegalArgumentException if the source or destination states are out of range
		 */
	def setQ(from: Int, to: Int, value: Double): Unit = {
		check(from, to, "setQ")
		qlData(from*numStates + to).value = value
	}
	
	final def get(from: Int, to: Int, varType: QLDataVar): String = {
		f"${qlData(from*numStates + to).value(varType)}%2.2f"
	}
   
		/**
		 * Retrieve the Q-value for an state transition action from 'state' from to state 'to'
		 * @param from Source state for the action
		 * @param to destination state for the action
		 * @throws IllegalArgumentException if the source or destination states are out of range
		 */
	final def Q(from: Int, to: Int): Double = {
		check(from, to, "Q")
		qlData(from*numStates + to).value
	}
	
		/**
		 * Retrieve the estimate for an state transition action from 'state' from to state 'to'
		 * @param from Source state for the action
		 * @param to destination state for the action
		 * @throws IllegalArgumentException if the source or destination states are out of range
		 */
	final def EQ(from: Int, to: Int): Double = {
		check(from, to, "EQ")
		qlData(from*numStates + to).estimate
	}
 
		/**
		 * Retrieve the reward for an state transition action from 'state' from to state 'to'
		 * @param from Source state for the action
		 * @param to destination state for the action
		 * @throws IllegalArgumentException if the source or destination states are out of range
		 */
	final def R(from: Int, to: Int): Double = {
		check(from, to, "R")
		qlData(from*numStates + to).reward
	}
   
		/**
		 * Retrieve the probability for an state transition action from 'state' from to state 'to'
		 * @param from Source state for the action
		 * @param to destination state for the action
		 * @throws IllegalArgumentException if the source or destination states are out of range
		 */
	final def P(from: Int, to: Int): Double = {
		check(from, to, "P")
		qlData(from*numStates + to).probability
	}
	
		/**
		 * Compute the minimum and maximum value for Q
		 * @return A pair (min Q-value, max Q-value)
		 */
	final def minMaxQ: DblPair = {
		val r = Range(0, numStates)
		val _min = r.minBy(from =>r.minBy(Q(from, _))) 
		val _max = r.maxBy(from =>r.maxBy(Q(from, _)))
		(_min, _max)
	}
 
		/**
		 * Retrieve the pair (index source state, index destination state) which transition
		 * is a positive value. The index of state is converted to a Double
		 * @return Array of tuple (source state, destination state)
		 */
	final def EQ: Vector[DblPair] = {
		import scala.collection.mutable.ArrayBuffer
		val r = Range(0, numStates)
		r.flatMap(from => 
				r.map(to => (from, to, Q(from, to))))
					.map{ case (i, j, q) => if(q > 0.0) (i.toDouble, j.toDouble) else (0.0, 0.0) }
					.toVector
	}
	
		/**
		 * Textual description of the reward matrix for this policy.
		 */
	override def toString: String = s"Reward\n${toString(REWARD)}"

		/**
		 * Textual representation of either the Q-value, reward or probability matrix
		 * @param varType type of variable (Q-value, reward, or probability)
		 */
	def toString(varType: QLDataVar): String = { 
		val r = Range(1, numStates)
		r.map(i => r.map(get(i, _, varType)).mkString(",")).mkString("\n")
	}

	
	private def check(from: Int, to: Int, meth: String): Unit =  {
		require(from >= 0 && from < numStates, 
				s"QLPolicy.$meth Found from: $from required >= 0 and < $numStates")
		require(to >= 0 && to < numStates, 
				s"QLPolicy.$meth Found to: $to required >= 0 and < $numStates")
	}
}


		/**
		 * Companion object to the QLPolicy class of Q-learning algorithm. The purpose of this
		 * singleton is to validate the class parameters and define its constructor
		 * 
		 * @author Patrick Nicolas
		 * @since January 25, 2014
		 * @note Scala for Machine Learning Chap 11 Reinforcement learning / Q-learning
		 */
object QLPolicy {
		/**
		 * Default constructor for a Q-learning policy
		 * @param input Input (rewards and probability) to initialize the policy.
		 */
	def apply[T](input: Seq[QLInput]): QLPolicy = new QLPolicy(input)

	private val MAX_NUM_TRANSITIONS = 32768

	protected def check(input: Seq[QLInput]): Unit = {
		require(input.nonEmpty && input.size < MAX_NUM_TRANSITIONS,
				s"QLPolicy found input size = ${input.size} requires 0 <  < $MAX_NUM_TRANSITIONS")
	}
}

// -----------------------------  EOF -----------------------------------
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
package org.scalaml.reinforcement.qlearning

import org.scalaml.util.Matrix




		/**
		 * <p>Enumerator to define the type of a parameters used to update the policy during
		 * the training of Q-learning model.</p>
		 */
object QLDataVar extends Enumeration {
	type QLDataVar = Value
	val REWARD, PROB, VALUE = Value
}


		/**
		 * <p>Class that encapsulate the attributes of the policy in Q-learning algorithm
		 * @author Patrick Nicolas
		 * @since January 25, 2014
		 * @note Scala for Machine Learning Chap 11 Reinforcement learning/Q-learning
		 */
final protected class QLData {
	import QLDataVar._
	var reward: Double = 1.0
	var probability: Double = 1.0
	var value: Double = 0.0
	
		/**
		 * Compute the overall value for an action adjusted by its probability
		 * @return adjusted value
		 */
	@inline
	final def estimate: Double = value*probability
	
		/**
		 * Select the attribute of an element of Q-learning policy using its type
		 * @param type of the attribute {REWARD, PROBABILITY, VALUE}
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
		 * <p>Class that defines the policy for a given set of input and a number of states.</p>
		 * @constructor Create a policy as a model for Q-learning. [numStates] Number of states for this policy. [input] Input to initialize the policy.
		 * @throws IllegalArgumentException if the class parameters are undefined
		 * @see org.scalaml.design.Model
		 * 
		 * @author Patrick Nicolas
		 * @since January 25, 2014
		 * @note Scala for Machine Learning Chap 11 Reinforcement learning/Q-learning
		 */
final protected class QLPolicy[T](numStates: Int, input: Array[QLInput]) {
	import QLDataVar._,  QLPolicy._
	check(numStates, input)
	
     
	val qlData = {
		val data = Array.tabulate(numStates)(v => Array.fill(numStates)(new QLData))
		input.foreach(in => {  
			data(in.from)(in.to).reward = in.reward
			data(in.from)(in.to).probability = in.prob
		})
		data
	}

   
	def setQ(from: Int, to: Int, value: Double): Unit = qlData(from)(to).value = value
   
	final def Q(from: Int, to: Int): Double = qlData(from)(to).value
	final def EQ(from: Int, to: Int): Double = qlData(from)(to).estimate
   
	final def R(from: Int, to: Int): Double = qlData(from)(to).reward
   
	final def P(from: Int, to: Int): Double = qlData(from)(to).probability
   
	override def toString: String = s"\nQ-Policy: Reward: ${toString(REWARD)}"

	def toString(varType: QLDataVar): String = { 
		val buf = new StringBuilder
		Range(1, numStates).foreach(i => {
			val line = qlData(i).zipWithIndex
						.foldLeft(new StringBuilder)((b, qj) => b.append(f"${qj._1.value(varType)}%1.2f,") )
			line.setCharAt(line.size-1, '\n')
			buf.append(line.toString)
		})
		buf.toString
	} 
}


		/**
		 * Companion object to the QLPolicy class of Q-learning algorithm. The purpose of this
		 * singleton is to validate the class parameters and define its constructor
		 */
object QLPolicy {
	final val MAX_NUM_STATES = 2048
	
	protected def check(numStates: Int, input: Array[QLInput]): Unit = {
		require(numStates >0 && numStates < MAX_NUM_STATES, s"QLPolicy.check Number of states $numStates is out of range")
		require(input != null, "QLPolicy.check the input to the Q-leaning policy is undefined")
		require(input.size > 0 && input.size < MAX_NUM_STATES, s"QLPolicy.check, the size of the input ${input.size} is out of range" )
	}
	
	def apply[T](numStates: Int, input: Array[QLInput]): QLPolicy[T] = new QLPolicy[T](numStates, input)
}


// -----------------------------  EOF -----------------------------------
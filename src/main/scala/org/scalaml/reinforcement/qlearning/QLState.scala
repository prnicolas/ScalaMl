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


import scala.util.Random
import org.scalaml.core.Types.ScalaMl._

		/**
		* State in the Q-learning. A state is uniquely defined by its identifier and the list of 
		* actions that transition from this state to another state. The list of actions is empty 
		* if this state is a goal. A state may have properties of type T that is independent from 
		* the state transition.
		* @tparam T type of the instance or object which states is managed by Q-learning algorithm
		* @constructor Create a state for Q-learning. 
		* @throws IllegalArgumentException if the id of the state is negative
		* @param id Identifier for the state.
		* @param actions List of actions for that transition from this state to other states with 
		* each action transition the model to single state.
		* @param instance Instance or object associated with this state.
		* @author Patrick Nicolas
		* @since 0.98 January 17, 2014
		* @see Scala for Machine Learning Chap 11 ''Reinforcement learning'' /Q-learning
		*/
@throws(classOf[IllegalArgumentException])
class QLState[T](val id: Int, val actions: Seq[QLAction] = List.empty, instance: T) {
	import QLState._
	check(id)
		/**
		 * Test if this state is a goal (or has not actions).
		 * @return true if the state has no actions, false otherwise
		 */
	@inline
	final def isGoal: Boolean = actions.nonEmpty
  
		/**
		 * Textual representation of a state in Q-learning. The state is defined
		 * by its Id and the list of the action it may potentially trigger
		 */
	override def toString: String = 
		s"state: $id ${actions.mkString(" ")}\nInstance: ${instance.toString}"
}



		/**
		 * Utility class to manage the state during the training phase
		 * @tparam T type of the instance or object which state is managed by Q-learning algorithm
		 * @param state current state in the search for the optimal policy
		 * @param iter number of iterations executed so far within this episode or epoch
		 * @author Patrick Nicolas
		 * @version 0.99.1
		 */
case class QLIndexedState[T](state: QLState[T], iter: Int)


		/**
		 * Companion object to the State class used for defining constructor and validating 
		 * its parameters
		 * @author Patrick Nicolas
		 * @since 0.98 January 17, 2014
		 * @see Scala for Machine Learning Chap 11 ''Reinforcement learning'' Q-learning
		 */
object QLState {
  
		/**
		 * Default constructor for QLState
		 * @tparam T type of the instance or object which states is managed by Q-learning algorithm
		 * @param id Identifier for the state.
		 * @param actions List of actions for that transition from this state to other states. 
		 * Each action transition the model to single state.
		 */
	def apply[T](id: Int, actions: Seq[QLAction], instance: T): QLState[T] = 
			new QLState(id, actions, instance)
  
	private def check[T](id: Int): Unit = 
		require( id >= 0, s"QLState found $id required id >= 0")
}

// ----------------------------  EOF --------------------------------------------------------------

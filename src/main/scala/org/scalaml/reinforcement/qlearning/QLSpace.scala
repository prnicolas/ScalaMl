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

	// Scala lib
import scala.collection._
import scala.collection.mutable.ListBuffer
import scala.util.Random
	// ScalaMl classes
import org.scalaml.core.Design.Model
import org.scalaml.core.Types.ScalaMl.DblArray
import org.scalaml.core.Types.emptyString
import org.scalaml.util.DisplayUtils
import QLConfig._



		/**
		 * Class that defines the search space (States x Actions) for the Q-Learning algorithm.
		 * The search space can be provided by the end user with a list of states and actions or
		 * automatically created by providing the number of states.
		 * 
		 * This implementation supports more than one goal per states space.
		 * @tparam T type of the instance or object which state is managed by Q-learning algorithm
		 * @constructor Create a state (or search) space .
		 * @throws IllegalArgumentException if either states or the goal(s) is undefined
		 * @param states States defined in the Q-learning search space.
		 * @param goalIds List of identifiers of states that are goals.
		 * @author Patrick Nicolas
		 * @since 0.98 January 17, 2014
		 * @version 0.99.1.1
		 * @see Scala for Machine Learning Chap 11 ''Reinforcement learning'' / Q-learning / 
		 * Implementation / The search space 
		 */
@throws(classOf[IllegalArgumentException])
protected class QLSpace[T](states: Seq[QLState[T]], goalIds: Array[Int])  {
	import QLSpace._
	check(states, goalIds)
	
		// Create a map of states as an immutable Map of state id and state instance
	private[this] val statesMap: immutable.Map[Int, QLState[T]] = states.map(st => (st.id, st)).toMap
	
		// Create a set (hash set) of id of the state goals. 
	private[this] val goalStates = new immutable.HashSet[Int]() ++ goalIds

		/**
		 * Compute the maximum value given a state and policy
		 * @param state State for which the maximum Q-value is computed
		 * @param policy Policy for which the maximum Q-value is computed
		 * @return Maximum Q-value
		 */
	final def maxQ(state: QLState[T], policy: QLPolicy): Double = {	
			// Select the states except this 'state' and select
			// the state that provides with the highest estimate given
			// the current 'policy'
		val best = states.filter( _ != state)
						.maxBy(st => policy.EQ(state.id, st.id))
		policy.EQ(state.id, best.id)
	}
	
			/**
			 * Access the number of states in the search space
			 * @return number of states
			 */
	@inline
	final def getNumStates: Int = states.size
    
		/**
		 * Select the initial state for a episode or epoch. The state is selected randomly
		 * if the 
		 * @param state0 Index of the initial state. If the index is invalid, then the 
		 * state is selected randomly.
		 * @return randomly selected state
		 */
	def init(state0: Int): QLState[T] = 
		if(state0 < 0) {
			val r = new Random(System.currentTimeMillis + Random.nextLong)
			states(r.nextInt(states.size-1))
		}
		else states(state0)

		/**
		 * Retrieve the list of the target states for action triggered by this state
		 * @param st state for which the target states are retrieved
		 * @return  List of target states (target to the action triggered from this state) if the
		 * current state has any action, an empty sequence of state otherwise 
		 */
	final def nextStates(st: QLState[T]): Seq[QLState[T]] = 
		if( st.actions.isEmpty ) 
			Seq.empty[QLState[T]] 
		else 
			st.actions.flatMap( ac => statesMap.get(ac.to) )
	

		/**
		 * Test if this state is a goal state
		 * @param state state that is test against goal
		 * @return true if this state is a goal state, false otherwise
		 */
	final def isGoal(state: QLState[T]): Boolean = goalStates.contains(state.id)
	
		/**
		 * Textual representation of this search or states space.
		 */
	override def toString: String = 
		s"\nGoals: ${goalIds.mkString(",")}\nStates ${states.mkString(" - ")}"
}


		/**
		 * Companion object for the Q-Learning search space. One of the constructor generates
		 * automatically the search space by randomizing the distribution of each state.
		 * 
		 * @author Patrick Nicolas
		 * @since January 22, 2014
		 * @note Scala for Machine Learning Chap 11 Reinforcement learning/Q-learning
		 */
object QLSpace {		

		/**
		 * Alternative constructor for a QLSpace which specifies the constraints on the selection
		 * of neighbors state from a given state.
		 * @tparam T type of the instance or object which state is managed by Q-learning algorithm
		 * @param goals Array of id of goal states
		 * @param instances Sequence of instances associated to this search or Q-learning algorithm
		 * @param constraints Function constraining the selection of neighbors states
		 * @return A new search space, QLSpace
		 * @throws IllegalArgumentException if one of the parameters is either undefined or out of range.
		 */
	@throws(classOf[IllegalArgumentException])
	def apply[T](
			goals: Array[Int], 
			instances: Seq[T],
			constraints: Option[Int => List[Int]]): QLSpace[T] = {
		
		require(goals.length >  0, 
		    s"QLSpace Found number of goals ${goals.length} required  > 0")
		require( instances.nonEmpty, "QLSpace.apply features are undefined")
		
		val r = instances.indices
		
			// Generate the states using the neighbors restrictive function
		val states = r.zipWithIndex.map{ case(x, n) =>
			
			// Generated the list of actions permitted by the neighbors restriction
			val validStates = constraints.map( _(n)).getOrElse(r)
			val actions = validStates.map(new QLAction(n, _)).filter(n != _.to)
				
			// Create a new state to be added to the state space or search space.
			QLState[T](n, actions, instances(n))
		}
		new QLSpace[T](states, goals)
	}
	
		
		/**
		 * Alternative constructor for a QLSpace for which the 
		 * @tparam T type of the instance or object which state is managed by Q-learning algorithm
		 * @param goal Id of the state selected as a goal
		 * @param instances Sequence of instances associated to this search or Q-learning algorithm
		 * @param constraints Function constraining the selection of neighbors states
		 * @return A new search space, QLSpace
		 * @throws IllegalArgumentException if one of the parameters is either undefined or out of range.
		 */
	def apply[T](
			goal: Int, 
			instances: Seq[T],
			constraints: Option[Int => List[Int]]): QLSpace[T] = 
		 apply(Array[Int](goal), instances, constraints)
		 
	
	private def check[T](states: Seq[QLState[T]], goalIds: Array[Int]): Unit = {
		require( states.nonEmpty,
				"QLSpace found empty sequence of states, required at least one state")
		require( goalIds.length > 0, 
				"QLSpace found undefined goals, required at least one goal")
	}
}


// ----------------------------  EOF --------------------------------------------------------------

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

	// Scala lib
import scala.collection._
import scala.collection.mutable.ListBuffer
import scala.util.Random
	// ScalaMl classes
import org.scalaml.core.Design.Model
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.util.DisplayUtils


		/**
		 * <p>Class that defines the search space (States x Actions) for the Q-Learning algorithm.
		 * The search space can be provided by the end user with a list of states and actions or
		 * automatically created by providing the number of states.<br>
		 * This implementation supports more than one goal per states space.</p>
		 * @constructor Create a state (or search) space. 
		 * @throws IllegalArgumentException if either states or the goal(s) is undefined
		 * @param states States defined in the Q-learning search space.
		 * @param goalIds List of identifiers of states that are goals.
		 * @author Patrick Nicolas
		 * @since January 17, 2014
		 * @note Scala for Machine Learning Chap 11 Reinforcement learning/Q-learning
		 */
protected class QLSpace[T](states: Array[QLState[T]], goalIds: Array[Int])  {
	import QLSpace._
	check(states, goalIds)
		// Create a map of states as an immutable Map of state id and state instance
	private[this] val statesMap: immutable.Map[Int, QLState[T]] = states.map(st => (st.id, st)).toMap
	
		// Create a set (hash set) of id of the state goals. 
	private[this] val goalStates = new immutable.HashSet[Int]() ++ goalIds

		/**
		 * <p>Compute the maximum value given a state and policy</p>
		 * @param state State for which the maximum Q-value is computed
		 * @param policy Policy for which the maximum Q-value is computed
		 * @return Maximum Q-value
		 * @throws IllegalArgumentException if either the state  or the policy is undefined
		 */
	final def maxQ(state: QLState[T], policy: QLPolicy[T]): Double = {	
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
		 * <p>Select a state randomly from the existing states</p>
		 * @param r Random generator
		 * @return randomly selected state
		 */
	def init(r: Random): QLState[T] = states(r.nextInt(states.size-1))
   
		/**
		 * <p>Retrieve the list of the target states for action triggered by this state</p>
		 * @param st state for which the target states are retrieved
		 * @return  List of target states (target to the action triggered from this state)
		 * @throws IllegalArgumenException if either the state's actions is undefined
		 */
	final def nextStates(st: QLState[T]): List[QLState[T]] = {
		require(!st.actions.isEmpty, "QLSpace.nextStates actions associated to this state undefined")
		st.actions.map(ac => statesMap.get(ac.to).get )
	}

		/**
		 * <p>Test if this state is a goal state</p>
		 * @param state state that is test against goal
		 * @return true if this state is a goal state, false otherwise
		 */
	final def isGoal(state: QLState[T]): Boolean = goalStates.contains(state.id)
	
		/**
		 * Textual representation of this search or states space.
		 */
	override def toString: String = 
		new StringBuilder("States\n")
			.append( states.foldLeft(new StringBuilder)(( buf, st) => {
				val isGoal = if(goalStates.contains(st.id) ) "(G)" else ""
				buf.append(s"$st $isGoal\n")
		}).toString ).toString
}


		/**
		 * <p>Companion object for the Q-Learning search space. One of the constructor generates
		 * automatically the search space by randomizing the distribution of each state.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since January 22, 2014
		 * @note Scala for Machine Learning Chap 11 Reinforcement learning/Q-learning
		 */
object QLSpace {		
		/**
		 * <p>Create a search space automatically using a scale factor.</p>
		 * @param numStates Number of symbols or states used by the Q-Learning algorithm
		 * @param goals Array of id of goal states
		 * @param features Features set 
		 * @param neighbors Function constrain to select neighboring states
		 * @return A new search space, QLSpace
		 * @throws IllegalArgumentExcetpion if one of the parameters is either undefined or out of range.
		 */
	def apply[T](
			numStates: Int, 
			goals: Array[Int], 
			features: Set[T], 
			neighbors: (Int, Int) => List[Int]): QLSpace[T] = {
		
		require(numStates >=0, s"QLSpace.apply The number of states $numStates should be positive")
		require( !features.isEmpty, "QLSpace.apply features are undefined")
		
			// Generate the states using the neighbors restrictive function
		val states = features.zipWithIndex
						.map(x => {	
								// Generated the list of actions permitted by the neighbors restriction
								val actions = neighbors(x._2, numStates).map(j =>new QLAction[T](x._2, j))
																.filter(x._2 != _.to)
								// Create a new state to be added to the state space or search space.
								QLState[T](x._2, actions, x._1)
						})
		new QLSpace[T](states.toArray, goals)
	}
	

		/**
		 * <p>Create a search space automatically using a scale factor.</p>
		 * @param numStates Number of symbols or states used by the Q-Learning algorithm
		 * @param goal Index of the state goal
		 * @param features Features set 
		 * @param neighbors Function constrain to select neighboring states
		 * @return A new search space, QLSpace
		 * @throws IllegalArgumentException if one of the parameters is either undefined or out of range.
		 */
	def apply[T](
			numStates: Int, 
			goal: Int, 
			features: immutable.Set[T], 
			neighbors: (Int, Int) => List[Int]): QLSpace[T] = 
		 apply(numStates, Array[Int](goal), features, neighbors)
		 
	
	private def check[T](states: Array[QLState[T]], goalIds: Array[Int]): Unit = {
		require( !states.isEmpty, 
				"QLSpace.check States list for QLSpace is undefined")
		require( !goalIds.isEmpty, 
				"QLSpace.check  List of goal states for QLSpace is undefined")
	}
}


// ----------------------------  EOF --------------------------------------------------------------

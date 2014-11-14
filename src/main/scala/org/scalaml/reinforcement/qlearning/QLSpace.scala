/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95d
 */
package org.scalaml.reinforcement.qlearning


import org.scalaml.util.Matrix
import org.scalaml.core.types.ScalaMl._
import org.scalaml.core.design.Model
import org.scalaml.util.Display

import scala.collection.immutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.util.Random


		/**
		 * <p>Class that defines the search space (States x Actions) for the Q-Learning algorithm.
		 * The search space can be provided by the end user with a list of states and actions or
		 * automatically created by providing the number of states.</p>
		 * @constructor Create a state (or search) space. [states] States comprising the search space. [goalIds] List of ids of states that are goals.
		 * @throws IllegalArgumentException if states or actions list are not well defined
		 * 
		 * @author Patrick Nicolas
		 * @since January 17, 2014
		 * @note Scala for Machine Learning Chap 11 Reinforcement learning/Q-learning
		 */
protected class QLSpace[T](states: Array[QLState[T]], goalIds: Array[Int])  {
	import QLSpace._
	check(states, goalIds)
	
	
	private[this] val statesMap: Map[Int, QLState[T]] = states.map(st => (st.id, st)).toMap
	private[this] val goalStates = new HashSet[Int]() ++ goalIds

	final def maxQ(state: QLState[T], policy: QLPolicy[T]): Double = {
		val best = states.filter( _ != state)
						.maxBy(st => policy.EQ(state.id, st.id))
		policy.EQ(state.id, best.id)
	}
    
	def init(r: Random): QLState[T] = states(r.nextInt(states.size-1))
   
	final def nextStates(st: QLState[T]): List[QLState[T]] = st.actions.map(ac => statesMap.get(ac.to).get )

	@inline
	final def isGoal(state: QLState[T]): Boolean = goalStates.contains(state.id)

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
		 * @param number of symbols or states used by the Q-Learning algorithm
		 */
	def apply[T](numStates: Int, goals: Array[Int], features: Set[T], neighbors: (Int, Int) => List[Int]): QLSpace[T] = {
		val states = features.zipWithIndex
						.map(x => {
							val actions = neighbors(x._2, numStates).map(j =>  new QLAction[T](x._2, j))
																.filter(x._2 != _.to)
							QLState[T](x._2, actions, x._1)
						})

		new QLSpace[T](states.toArray, goals)
	}
	
	
	def apply[T](numStates: Int, goal: Int, features: Set[T], neighbors: (Int, Int) => List[Int]): QLSpace[T] = 
		 apply(numStates, Array[Int](goal), features, neighbors)
		 

	private def check[T](states: Array[QLState[T]], goalIds: Array[Int]): Unit = {
		require(states != null && states.size > 1, "States list for QLSpace is undefined")
		require(goalIds != null && goalIds.size > 0, "List of goal states for QLSpace is undefined")
	}
}


// ----------------------------  EOF --------------------------------------------------------------

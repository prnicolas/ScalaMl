/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.reinforcement.qlearning

import scala.util.Random
import org.scalaml.util.Matrix
import org.scalaml.core.Types.ScalaMl._

import QLearning._


	 /**
	 * <p>Class that defines the search space (States x Actions) for the Q-Learning algorithm.
	 * The search space can be provided by the end user with a list of states and actions or
	 * automatically created by providing the number of states.</p>
	 * @param states list of states for the search space
	 * @param actions list of actions for the search space
	 * @throws IllegalArgument if states or actions list are not well defined
	 * 
	 * @author Patrick Nicolsa
	 * @since January 17, 2014
	 * @note Scala for Machine Learning
	 */
class QLSpace[T <% Double](val states: List[QLState[T]], val actions: List[QLAction[T]]) {
   require(states != null && states.size > 1, "States list is undefined")
   require(actions != null && actions.size > 1, "Action list is undefined")

   val statesActions = (states, actions)
}


	/**
	 * <p>Companion object for the Q-Learning search space. One of the constructor generates
	 * automatically the search space by randomizing the distribution of each state.</p>
	 * 
	 * @author Patrick Nicolas
	 * @since January 22, 2014
	 */
object QLSpace {
	  final val SCALE_FACTOR = 1000
		
		/**
		 * <p>Create a search space automatically using a scale factor.</p>
		 * @param number of symbols or states used by the Q-Learning algorithm
		 */
	def apply[T <% Double](numStates: Int): QLSpace[T] = {
	  require( numStates > 0, "Cannot create a search space for QLearning with undefined number of states")

	  	// Inner function
	  def createNextStates(startIndex: Int, value: Int, zeroStart: Boolean = true): List[QLState[T]] = {
	  	  Range(startIndex+1, numStates).zipWithIndex
	  	                                 .foldLeft(List[QLState[T]]()) ((s, n) => { 
	  	     val dist = Array.fill(numStates)(value)
	  	  	 if(zeroStart) 
	  	  		  dist(startIndex) = 0
	  	  	 dist(n._1) = 0
	  	  	 QLState[T](n._2, dist) :: s
	  	  })
      }
  	  val avgdistribution = (SCALE_FACTOR/numStates).floor.toInt
  	  val allStates = QLState[T](0, Array.fill(numStates)(avgdistribution)) :: List[QLState[T]]()
  	  val value_1 = (SCALE_FACTOR/(numStates-1)).floor.toInt
  	  val value_2 = (SCALE_FACTOR/(numStates-2)).floor.toInt
  	  
  	  val nextStatesList = createNextStates(0, value_1, false)
  	  val combinedStates = nextStatesList ::: allStates
  	  val statesList = nextStatesList.zipWithIndex.foldLeft(combinedStates)((xs, st) => createNextStates(st._2, value_2) ::: xs)

  	  new QLSpace[T](statesList, statesList.foldLeft(List[QLAction[T]]()) ((acs, st) => QLAction(st, statesList) :: acs))
   }
	
	def apply[T <% Double](states: List[QLState[T]], actions: List[QLAction[T]]): QLSpace[T] = new QLSpace[T](states, actions)
}


// ----------------------------  EOF --------------------------------------------------------------


    

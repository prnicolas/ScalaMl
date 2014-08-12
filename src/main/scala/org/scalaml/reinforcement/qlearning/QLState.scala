/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.reinforcement.qlearning

import scala.collection.mutable.ListBuffer
import scala.util.Random
import org.scalaml.util.Matrix
import scala.collection.mutable.HashMap
import org.scalaml.util.Accumulator
import org.scalaml.core.Types


			/**
			 * <p>State defines as a label and a normalized values or frequency disribution across a set
			 * of categories or symbols. The main purpose of the state class is to compute its
			 * Value V.</p>
			 * @param id optional label or identifier for the state. The label is used as a key
			 * @param distribution normalized distribution of values per categories
			 * @param cost default cost function used to compute the V value of the state
			 * @throws IllegalArgument is the normalized distribution is not defined
			 * 
			 * @author Patrick Nicolas
			 * @since January 17, 2014
			 * @note Scala for Machine Learning
			 */

import Types.ScalaMl._
class QLState[T <% Double](val id: Int, val distribution: DblVector, val cost: (T, Double) => Double) {
  require(distribution != null && distribution.size > 0, "Cannot create a state with undefined distribution")
  require(cost != null, "Cannot evaluate this state with undefined cost function")
  		
  def == (that: QLState[T]): Boolean = that.id == id
  def != (that: QLState[T]): Boolean = ! ==(that)
  
  		/**
  		 * Compute the value of the state V using the distribution
  		 * @param data observations used to compute the value V of this state
  		 * @throws IllegalArgumentException if the observations is undefined or the number 
  		 * of observations is different from the dimension of the model (distribution)
  		 * @return V-value of the state
  		 */
  def V(data: DVector[T]): Double =  {
  	require(data != null && data.size > 0, "Cannot compute value of this state with undefined")
  	require(data.size == distribution.size, "Cannot compute value of state for data size " + data.size + " differs from distribution size " + distribution.size)
  	data.zipWithIndex.foldLeft(0.0)((s, x) => s + cost(x._1, distribution(x._2)))
  }
  
  override def toString: String = 
     new StringBuilder(id.toString)
	        .append("=>")
		        .append(distribution.foldLeft(new StringBuilder)((b,n) => b.append(n).append(","))).toString
}


		/**
		 * Companion object to the State class used for defining constructors
		 */
object QLState {
  def apply[T <% Double](id: Int, distribution: DblVector, cost: (T, Double) => Double): QLState[T] = new QLState(id, distribution, cost)
  def apply[T <% Double](id: Int, distribution: DblVector): QLState[T] = new QLState(id, distribution,  (t:T, x: Double) => t*x)
  
  		/**
  		 * Method to create a state with random distribution.
  		 */
  def createRandomState[T <% Double](id: Int, distributionSize: Int): QLState[T] =  {
  	 val distribution = Array.fill(distributionSize)(0.5).map(_ => Random.nextDouble)
  	 val sum = distribution.sum
  	 QLState(id, distribution.map( _ /sum))
  }
}


		/**
		 * <p>Class that define action between on source state and multiple
		 * destination states.</p>
		 * @param from Source state
		 * @param to list of destination states
		 * @throws IllegalArgumentException if the source or destination states are undefined.
		 * 
		 * @author Patrick Nicolas
		 * @since January, 22, 2014
		 */
class QLAction[T <% Double](val from: QLState[T], val to: List[QLState[T]]) {
   require(from != null, "Cannot create an action for Q-Learning with indefined source state")	
   require(to != null && to.size > 0, "Cannot create an action for Q-Leaning with indefined destination states")	
	
   override def toString: String = 
      new StringBuilder(from.id.toString) 
            .append(": ")
               .append(to.foldLeft(new StringBuilder)((b,x) => b.append(x.id).append(" "))).toString
}




object QLAction {
  def apply[T <% Double](from: QLState[T], to: List[QLState[T]]): QLAction[T] = new QLAction[T](from, to)
  
}



// ----------------------------  EOF --------------------------------------------------------------


    

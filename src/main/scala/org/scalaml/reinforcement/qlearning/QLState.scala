/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.reinforcement.qlearning


import scala.util.Random
import org.scalaml.core.types.ScalaMl._

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
class QLState[T](val id: Int, val actions: List[QLAction[T]] = List.empty, prop: T) {
  require(actions != null, "Cannot create a QLState with undefined list of actions")

  def == (that: QLState[T]): Boolean = that.id == id
  def != (that: QLState[T]): Boolean = ! ==(that)
  
  @inline
  final def hasActions: Boolean = actions != List.empty
  
  override def toString: String = 
     new StringBuilder(s"state: $id ")
		    .append( actions.foldLeft(new StringBuilder)((b,a) => b.append(s"$a ")).toString )
		    .toString
}


		/**
		 * Companion object to the State class used for defining constructors
		 */
object QLState {
  def apply[T](id: Int, actions: List[QLAction[T]], t: T): QLState[T] = new QLState(id, actions, t)
  
  protected def check[T](id: Int, actions: List[QLAction[T]], prop: T): Unit = {
    
  }
}



// ----------------------------  EOF --------------------------------------------------------------


    

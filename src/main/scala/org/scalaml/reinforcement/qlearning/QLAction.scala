/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.reinforcement.qlearning


		/**
		 * <p>Class that define action between on source state and multiple
		 * destination states.</p>
		 * @param from Source state
		 * @param to list of destination states
		 * @throws IllegalArgumentException if the source or destination states are undefined.
		 * 
		 * @author Patrick Nicolas
		 * @since January, 22, 2014
		 * @note Scala for Machine Learning Chapter 11 Reinforcement learning/Q-learning
		 */
class QLAction[T](val from: Int, val to: Int) {
   override def toString: String = s"\naction: state ${from} => state ${to}"
}


// ----------------------------  EOF --------------------------------------------------------------


    

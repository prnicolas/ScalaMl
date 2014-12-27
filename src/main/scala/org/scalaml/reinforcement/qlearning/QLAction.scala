/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.reinforcement.qlearning


		/**
		 * <p>Class that define action between on source state and multiple
		 * destination states.<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;
		 * ,Arial,Helvetica,sans-serif;">
		 * <b>from</b>   Source state
		 * <b>to</b>     list of destination states
		 * </span></pre></p>
		 * @constructor Create an state transition action for the Q-learning algorithm.
		 * @throws IllegalArgumentException if the source or destination states ir undefined.
		 * @param from Source state in the transition (source of the action)
		 * @param to List of destination states in the transition (target of the action)
		 * 
		 * @author Patrick Nicolas
		 * @since January, 22, 2014
		 * @note Scala for Machine Learning Chapter 11 Reinforcement learning / Q-learning
		 */
final protected class QLAction[T](val from: Int, val to: Int) {
	require(from >= 0, s"QLAction index $from of source state is undefined")
	require(to >= 0, s"QLAction index $to of source state is undefined")

	override def toString: String = s"\naction: state ${from} => state ${to}"
}


// ----------------------------  EOF --------------------------------------------------------------

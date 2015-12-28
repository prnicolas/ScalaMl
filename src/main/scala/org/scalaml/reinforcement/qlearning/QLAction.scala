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
 * Version 0.99
 */
package org.scalaml.reinforcement.qlearning


		/**
		 * Class that define action between on source state and multiple
		 * destination states.
		 * - '''from'''   Source state
		 * - '''to''' list of destination states
		 * @constructor Create an state transition action for the Q-learning algorithm.
		 * @throws IllegalArgumentException if the source or destination states ir undefined.
		 * @param from Source state in the transition (source of the action)
		 * @param to List of destination states in the transition (target of the action)
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98 January, 22, 2014
		 * @see Scala for Machine Learning Chapter 11 "Reinforcement learning" / Q-learning
		 */
@throws(classOf[IllegalArgumentException])
protected case class QLAction(val from: Int, val to: Int) {
	require(from >= 0, s"QLAction found from: $from required: >=0")
	require(to >= 0, s"QLAction found to: $to required: >=0")

	override def toString: String = s"\nAction: state ${from} => state ${to}"
}


// ----------------------------  EOF --------------------------------------------------------------

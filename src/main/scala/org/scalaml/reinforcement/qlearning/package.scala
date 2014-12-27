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
package org.scalaml.reinforcement

		/**
		 * This package object encapsulates implementation of Q-learning algorithms. It is
		 * defined by the following classes<br>
		 * - State transition actions <b>QLAction</b><br>
		 * - Configuration parameters for the execution the Q-learning algorithm (training and
		 * state prediction) <b>QLConfig</b><br>
		 * - State and its associated actions <b>QLState</b><br>
		 * - Search space of all states and state transitions <b>QLSpace</b><br>
		 * - Policy defines as the value, probability and reward matrices <b>QLPolicy</b><br>
		 * - Model generated through training containing the most rewarding policy and training
		 * coverage <b>QLModel</b><br>
		 * - Probability and reward input to policy <b>QLInput</b><br>
		 * - Q-learning algorithm for the generation of the optimum policy and prediction for
		 * next state <b>QLearning</b><br>
		 *  @note Scala for Machine Learning Chapter 11 Reinforcement learning / Q-learning
		 */
package object qlearning { }
// ---------------------------------------  EOF -----------------------------------------
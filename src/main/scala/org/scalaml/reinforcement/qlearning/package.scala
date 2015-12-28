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
package org.scalaml.reinforcement

		/**
		 * This package object encapsulates implementation of Q-learning algorithms. It is
		 * defined by the following classes
		 *
		 * - State transition actions '''QLAction'''
		 * 
		 * - Configuration parameters for the execution the Q-learning algorithm (training and
		 * state prediction) '''QLConfig'''
		 * 
		 * - State and its associated actions '''QLState'''
		 * 
		 * - Search space of all states and state transitions '''QLSpace'''
		 * 
		 * - Policy defines as the value, probability and reward matrices '''QLPolicy'''
		 * 
		 * - Model generated through training containing the most rewarding policy and training
		 * coverage '''QLModel'''
		 * 
		 * - Probability and reward input to policy '''QLInput'''
		 * 
		 * - Q-learning algorithm for the generation of the optimum policy and prediction for
		 * next state '''QLearning'''
		 * 
		 * @see Scala for Machine Learning Chapter 11 ''Reinforcement learning'' / Q-learning
		 */
package object qlearning { }
// ---------------------------------------  EOF -----------------------------------------
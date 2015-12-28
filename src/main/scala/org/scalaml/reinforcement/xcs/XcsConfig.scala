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
package org.scalaml.reinforcement.xcs


import org.scalaml.ga.GAConfig
import org.scalaml.reinforcement.qlearning.QLConfig
import org.scalaml.core.Design.Config


		/**
		 * Class that defined the configuration parameters for the XCS algorithm.
		 * @constructor Create a configuration for the XCS algorithm 			
		 * @param gaConfig  Configuration of the Genetic Algorithm used in extracting the 
		 * fittest rules or classifier
		 * @param qlConfig  Configuration parameters for the Q-learning algorithm.
		 * 
		 * @author Patrick Nicolas
		 * @since March 22, 2014
		 * @note Scala for Machine Learning Chapter 11 Reinforcement learning / 
		 * Extended learning classifier systems
		 */
final protected class XcsConfig(val gaConfig: GAConfig, val qlConfig: QLConfig) extends Config


// --------------------------  EOF ------------------------------------------------
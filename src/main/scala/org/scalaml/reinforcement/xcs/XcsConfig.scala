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
package org.scalaml.reinforcement.xcs


import org.scalaml.ga.GAConfig
import org.scalaml.reinforcement.qlearning.QLConfig
import org.scalaml.core.Design.Config


		/**
		 * <p>Class that defined the configuration parameters for the XCS algorithm.</p>
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
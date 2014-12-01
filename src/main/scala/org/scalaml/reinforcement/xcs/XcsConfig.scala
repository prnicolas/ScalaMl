/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96c
 */
package org.scalaml.reinforcement.xcs

import scala.io.Source

import org.scalaml.ga.{Chromosome, GAConfig, Population}
import org.scalaml.trading.Signal
import org.scalaml.reinforcement.qlearning.QLConfig


		/**
		 * <p>Class that defined the configuration parameters for the XCS algorithm.</p>
		 * @constructor Create a configuration for the XCS algorithm 			
		 * @throws IllegalArgumentException if the maximum population is out of range or one of the
		 * configuration class is undefined.
		 * @param gaConfig  Configuration of the Genetic Algorithm used in extracting the fittest rules or classifier
		 * @param qlConfig  Configuration parameters for the Q-learning algorithm
		 * @author Patrick Nicolas
		 * @since March 22, 2014
		 * @note Scala for Machine Learning Chapter 11 Reinforcement learning/Extended learning classifier systems
		 */
final protected class XcsConfig(val gaConfig: GAConfig, val qlConfig: QLConfig) {
	require(gaConfig != null, "XcsConfig.check Cannot initialize XCS algorithm with undefined GA configuration")
	require(qlConfig != null, "XcsConfig.check Cannot initialize XCS algorithm with undefined Q-Learning configuration")
}


// --------------------------  EOF ------------------------------------------------
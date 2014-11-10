/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95c
 */
package org.scalaml.reinforcement.xcs

import scala.io.Source

import org.scalaml.ga.{Chromosome, GAConfig, Population}
import org.scalaml.trading.Signal
import org.scalaml.reinforcement.qlearning.QLConfig


		/**
		 * <p>Class that defined the stateuration	parameters for the XCS algorithm.</p>
		 * @param maxPopulation  maximum size of the population used to constraint the execution
		 * of the genetic algorithm
		 * @param gaConfig stateuration of the genetic algorithm
		 * @param qConfig stateuration of the Q-Learning algorithm. 
		 * @param init function to initialize the population of rules (Boost) 			
		 * @throws IllegalArgumentException if the maximum population is out of range or one of the
		 * stateuration class is undefined.
		 *  
		 * @author Patrick Nicolas
	     * @since March 22, 2014
		 * @note Scala for Machine Learning
		 */

class XcsConfig(val gaConfig: GAConfig, val qlConfig: QLConfig) {
	require(gaConfig != null, "Cannot initialize XCS algorithm with undefined GA configuration")
	require(qlConfig != null, "Cannot initialize XCS algorithm with undefined Q-Learning configuration")
}


// --------------------------  EOF ------------------------------------------------
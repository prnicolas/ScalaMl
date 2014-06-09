/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.reinforcement.xcs

import scala.io.Source

import org.scalaml.ga.{Chromosome, GAConfig, Population}
import org.scalaml.trading.Signal
import org.scalaml.reinforcement.qlearning.QLConfig



		/**
		 * <p>Class that defined the configuration	parameters for the XCS algorithm.</p>
		 * @param maxPopulation  maximum size of the population used to constraint the execution
		 * of the genetic algorithm
		 * @param gaConfig configuration of the genetic algorithm
		 * @param qConfig configuration of the Q-Learning algorithm. 
		 * @param init function to initialize the population of rules (Boost) 			
		 * @exception IllegalArgumentException if the maximum population is out of range or one of the
		 * configuration class is undefined.
		 *  
		 * @author Patrick Nicolas
	     * @date March 22, 2014
		 * @project Scala for Machine Learning
		 */
import XcsConfig._
class XcsConfig(val maxPopulationSize: Int, val gaConfig: GAConfig, val qConfig: QLConfig, val init: () => Population[XcsRule[Double]]) {
	require(maxPopulationSize > 0 && maxPopulationSize < MAX_POPULATION_SIZE, "Maximum size of population for XCS " + maxPopulationSize + " is out of range")
	require(gaConfig != null, "Cannot initialize XCS algorithm with undefined GA configuration")
	require(qConfig != null, "Cannot initialize XCS algorithm with undefined Q-Learning configuration")
	require(init != null, "Cannot training XCS without initializing the population of rules")
}


object XcsConfig {
	final val MAX_POPULATION_SIZE = 10000
}



// --------------------------  EOF ------------------------------------------------
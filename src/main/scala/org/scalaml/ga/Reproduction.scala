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
package org.scalaml.ga

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.annotation.switch
	
import org.scalaml.core.Types.ScalaMl.{DblArray, DblMatrix}
import org.scalaml.util.DisplayUtils
import Chromosome._

		/**
		 *  Define the replication cycle in the execution of the genetic algorithm optimizer.
		 *  A replication cycle consists of a selection of chromosomes according to their 
		 *  fitness/unfitness values, cross-over of pair of chromosomes and mutation
		 *  
		 *  @constructor Create a reproduction cycle for the genetic algorithm. 
		 *  @tparam T type of gene (inherited from '''Gene''')
		 *  @param score Scoring function of a chromosome (unfitness of the candidate solution)
		 *  @author Patrick Nicolas
		 *  @since August 28, 2013
		 *  @version 0.98.2
		 *  @see Scala for Machine Learning Chapter 10 ''Genetic Algorithm'' / Genetic algorithm 
		 *  components
		 */
final protected class Reproduction[T <: Gene](score: Chromosome[T] => Unit) { 	   
	private[this] val rand = new Random(System.currentTimeMillis)

		/**
		 * Execute the 3 phases of the genetic replication: Selection, Cross-over and Mutation.
		 * @param population current population of chromosomes used in the replication process
		 * @param config configuration of the genetic algorithm.
		 * @param cycle Current reproduction cycle number
		 * @return true if the selection, crossover and mutation phases succeed, None otherwise.
		 */
	def mate(
			population: Population[T], 
			config: GAConfig, 
			cycle: Int): Boolean = (population.size: @switch) match {
	
			// If the population has less than 3 chromosomes, exit
		case 0 | 1 | 2 => false  
			// Otherwise execute another reproduction cycle, starting with selection
		case _ => {
			rand.setSeed(rand.nextInt + System.currentTimeMillis)
			population.select(score, config.softLimit(cycle))		//1. Selection
			population +- rand.nextDouble*config.xover			//2. Cross-over
			population ^ rand.nextDouble*config.mu					//3. Mutation
			true
		}
	}
}


		/**
		 * Companion object for the Reproduction class. This singleton is used
		 * to define the default constructor of the Reproduction class.
		 *  @author Patrick Nicolas
		 *  @since August 28, 2013
		 *  @see Scala for Machine Learning Chapter 10 ''Genetic Algorithm'' / Genetic algorithm 
		 *  components
		 */
object Reproduction {
	
		/**
		 * Default constructor for a reproduction cycle
		 * @param score Scoring function of a chromosome (unfitness of the candidate solution)
		 */
	def apply[T <: Gene](score: Chromosome[T] => Unit): Reproduction[T] = new Reproduction[T](score)
}

// ------------------------------  EOF --------------------------------------------------
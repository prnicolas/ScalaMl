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
package org.scalaml.ga

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import org.scalaml.core.Types.ScalaMl.{DblVector, DblMatrix}
import org.scalaml.core.XTSeries
import org.scalaml.util.DisplayUtils
import Chromosome._

		/**
		 * <p>Define the replication cycle in the execution of the genetic algorithm optimizer.
		 *  A replication cycle consists of a selection of chromosomes according to their 
		 *  fitness/unfitness values, cross-over of pair of chromosomes and mutation<br>
		 *  <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;
		 *  ,Arial,Helvetica,sans-serif;">
		 *  <b>score</b>   Scoring function of a chromosome.
		 *  </span></pre></p>
		 *  @constructor Create a reproduction cycle for the genetic algorithm. 
		 *  @param score Scoring function of a chromosome (unfitness of the candidate solution)
		 *  @author Patrick Nicolas
		 *  @since August 28, 2013
		 *  @note Scala for Machine Learning Chapter 10 Genetic Algorithm / Genetic algorithm components
		 */
final protected class Reproduction[T <: Gene](score: Chromosome[T] => Unit) { 	   
	
		/**
		 * <p>Execute the 3 phases of the genetic replication: Selection, Cross-over and Mutation.</p>
		 * @param population current population of chromosomes used in the replication process
		 * @param config configuration of the genetic algorithm.
		 * @param cycle Current reproduction cycle number
		 * @return true if the selection, crossover and mutation phases succeed, None otherwise.
		 */
	import scala.annotation.switch
	def mate(population: Population[T], 
			config: GAConfig, 
			cycle: Int): Boolean = (population.size: @switch) match {
			// If the population has less than 3 chromosomes, exit
		case 0 | 1 | 2 => false  
			// Otherwise execute another reproduction cycle, starting with selection
		case _ => {
			population.select(score, config.softLimit(cycle))		//1. Selection
			population +- (1.0 - Random.nextDouble*config.xover)	//2. Cross-over
			population ^ (1.0 -  Random.nextDouble*config.mu)		//3. Mutation
			true
		}
	}
}


		/**
		 * Companion object for the Reproduction class. This singleton is used
		 * to define the default constructor of the Reproduction class.
		 *  @author Patrick Nicolas
		 *  @since August 28, 2013
		 *  @note Scala for Machine Learning Chapter 10 Genetic Algorithm / Genetic algorithm components
		 */
object Reproduction {
	
		/**
		 * Default constructor for a reproduction cycle
		 * @param score Scoring function of a chromosome (unfitness of the candidate solution)
		 */
	def apply[T <: Gene](score: Chromosome[T] => Unit): Reproduction[T] = new Reproduction[T](score)
}

// ------------------------------  EOF --------------------------------------------------
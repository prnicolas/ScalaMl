/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95d
 */
package org.scalaml.ga

import Chromosome._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import org.scalaml.core.types.ScalaMl.{DblVector, DblMatrix}
import org.scalaml.core.XTSeries
import org.scalaml.util.Display

		/**
		 * <p>Define a replication phase in the execution of the genetic algorithm optimizer. The
		 *  A replication sequence consists of a selection of chromosomes according to their 
		 *  fitness values, cross-over within a pair of chromosomes and mutation</p>
		 *  @constructor Create a reproduction workflow for the genetic algorithm. [fit] Fitness function for a chromosome, [xt] times series used in the scoring, fitness or objective function.
		 *  @param fit fitness function for a chromosome
		 *  @throws IllegalArgumentException or the fitness function is undefined
		 *  		 
		 *  @author Patrick Nicolas
		 *  @since August 28, 2013
		 *  @note Scalal for Machine Learning Chapter 10 Genetic Algorithm/Genetic algorithm components
		 */

final class Reproduction[T <: Gene](score: Chromosome[T] => Unit) { 	   
    require(score != null, "The chromosome fitness function of GA is undefined")
    		/**
    		 * <p>Execute the 3 phases of the genetic replication: Selection, Cross-over and Mutation.</p>
    		 * @param population current population of chromosomes used in the replication process
    		 * @param config configuration of the genetic algorithm.
    		 * @return an update population if the selection, crossover and mutation phases succeed, None otherwise.
    		 */    
    def mate(population: Population[T], config: GAConfig, cycle: Int): Boolean = population.size match {
    	 case 0 | 1 | 2 => false
	     case _ => {
	    	  population.select(score, config.softLimit(cycle))
	    	  population +- (1.0 - Random.nextDouble*config.xover)
	    	  population ^ (1.0 -  Random.nextDouble*config.mu)
	    	  true
	     }
    }
}


object Reproduction {
   import scala.util.Random
   def apply[T <: Gene](score: Chromosome[T] =>Unit): Reproduction[T] = new Reproduction[T](score)
}

// ------------------------------  EOF --------------------------------------------------
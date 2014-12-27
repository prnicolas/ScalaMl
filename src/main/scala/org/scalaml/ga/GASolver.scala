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

import scala.util.Random

import org.apache.log4j.Logger

import org.scalaml.core.Design.PipeOperator
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl.{DblVector, DblMatrix}
import org.scalaml.ga.state._
import org.scalaml.util.DisplayUtils

import Chromosome._


		/**
		 * <p>Class to select the best solution or Chromosome from an initial population
		 * or genes pool using a set of policies for selection, mutation and crossover of
		 * chomosomes. The client code initialize the GA solver with either an initialized
		 * population or a function () => Population{T] that initialize the population. THe
		 * class has only one public method search.<br>
		 * Reference: http://www.kddresearch.org/Publications/Book-Chapters/Hs5.pdf</p>
		 * @constructor Create a generic GA-based solver. [state] Configuration parameters for the 
		 * GA algorithm, [population] Initialized population of chromosomes (solution candidates)
		 * @param config  Configuration parameters for the GA algorithm
		 * @param score Scoring method for the chromosomes of this population
		 * @throws IllegalArgumenException if the configuration is undefined or the population 
		 * is not initialized
		 * 
		 * @author Patrick Nicolas
		 * @since August 29, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
final protected class GASolver[T <: Gene](
		config: GAConfig, 
		score: Chromosome[T] => Unit) extends PipeOperator[Population[T], Population[T]] {
	import GAConfig._
	
		// Initial state of the genetic algorithm solver
	private[this] var state: GAState = GA_NOT_RUNNING
	private val logger = Logger.getLogger("GASolver")
	
		/**
		 * <p>Method to resolve any optimization problem using a function to generate
		 * a population of Chromosomes, instead an existing initialized population
		 * @param initialize Function to generate the chromosomes of a population
		 * @throws IllegalArgumenException If the initialization or chromosome generation function is undefined
		 */
	def |>(initialize: () => Population[T]): Population[T] = this.|>(initialize())
   
   
		/**
		 * <p>Uses the genetic algorithm reproduction cycle to select the fittest
		 * chromosomes (or solutions candidate) after a predefined number of reproduction cycles.<br>
		 * Convergence criteria used to end the reproduction cycle is somewhat dependent of the
		 * problem or domain. This implementation makes sense for the exercise in the book
		 * chapter 10. It needs to be modified to a specific application.</p>
		 * @throws MatchError if the population is empty
		 * @return PartialFunction with a parameterized population as input and the population 
		 * containing the fittest chromosomes as output.
		 */
	override def |> : PartialFunction[Population[T], Population[T]] = {	
		case population: Population[T] if(state != GA_RUNNING &&population.size > 1) => {
		
				// Create a reproduction cycle manager with a scoring function
		  val reproduction = Reproduction[T](score)
			state = GA_RUNNING

				// Trigger a reproduction cycle 'mate' then test if 
				// any of the convergence criteria applies.
			Range(0, config.maxCycles).find(n => {  		 
				if( reproduction.mate(population, config, n) )
					converge(population, n) != GA_RUNNING
				else {
					if(population.size == 0) 
						state = GA_FAILED(s"GASolver.PartialFunction reproduction failed after $n cycles") 
					else 
						state = GA_SUCCEED(s"GASolver.PartialFunction Completed in $n cycles")
					true
				}
			}).getOrElse(notConverge)
				// The population is returned no matter what..
			population
		}
	}
   

		/*
		 * Domain dependent convergence criteria. This version tests the size
		 * of the population and the number of reproduction cycles already executed
		 */
	private def converge(population: Population[T], cycle: Int): GAState = {
		if( population.isNull ) 
			GA_FAILED(s"GASolver.converge Reproduction failed at $cycle")
		else if(cycle >= config.maxCycles)
			GA_NO_CONVERGENCE(s"GASolver.converge Failed to converge at $cycle ")
		else
			GA_RUNNING
	}
	
	private def notConverge: Int = {
	  state = GA_NO_CONVERGENCE(s"GASolver.PartialFunction Failed to converge")
	  -11
	}
}


		/**
		 * Object companion for the Solve that defines the two constructors
		 * @author Patrick Nicolas
		 * @since August 29, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm
		 */
object GASolver {

		/**
		 * Default constructor for the Genetic Algorithm (class GASolver)
		 * @param config  Configuration parameters for the GA algorithm
		 * @param score Scoring method for the chromosomes of this population
		 */
	def apply[T <: Gene](config: GAConfig, score: Chromosome[T] =>Unit): GASolver[T] = 
			new GASolver[T](config, score)

		/**
		 * Constructor for the Genetic Algorithm (class GASolver) with undefined scoring function
		 * @param config  Configuration parameters for the GA algorithm
		 */
	def apply[T <: Gene](config: GAConfig): GASolver[T] = 
			new GASolver[T](config, (c: Chromosome[T]) => Unit)
}

// ---------------------------  EOF -----------------------------------------------
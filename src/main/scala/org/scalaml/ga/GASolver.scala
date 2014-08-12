/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.ga

import scala.util.Random


object GAState extends Enumeration {
   type GAState = Value
   val SUCCEED, FAILED, NO_CONVERGE, RUNNING = Value
}



	/**
	 * <p>Class to select the best solution or Chromosome from an initial population
	 * or genes pool using a set of policies for selection, mutation and crossover of
	 * chomosomes. The client code initialize the GA solver with either an initialized
	 * population or a function () => Population{T] that initialize the population. THe
	 * class has only one public method search.<br>
	 * Reference: http://www.kddresearch.org/Publications/Book-Chapters/Hs5.pdf</p>
	 * @param config Configuration parameters for the GA algorithm
	 * @param population initialized population of chromosomes (solution candidates)
	 * @throws IllegalArgumenException if the configuration is undefined or the population is not initialized
	 * 
	 * @author Patrick Nicolas
	 * @since August 29, 2013
	 * @note Scala for Machine Learning
	 * /
	 */
import Chromosome._
import GAState._
final class GASolver[T <: Gene](private val config: GAConfig, val population: Population[T]) {
   require(config != null, "GA configuration is undefined")
   require(population != null && population.size > 1, "Population for the GA solver is undefined")

	/**
	 * <p>Method to resolve any optimization problem using an initial genes pool 
	 * (population of Chromosomes. The minimum size of the population is 2</p>
	 * @param fitness fitness Function used to rank and select a chromosome.
	 * @throws IllegalArgumenException if the fitness function is not provided.
	 */
   def search(fit: Chromosome[T] => Double): GAState = {	
      require(fit != null, "Cannot search with an undefined fitness Function")

	  val reproduction = Reproduction[T](config, fit)
      var state = GAState.RUNNING
      var pop = population
        
      Range(0, config.maxNumIters).find( n => {  
         val prev: Population[T] = pop
		 pop = reproduction.mate(prev).getOrElse(null)		
	     state = converge(population, prev, n)
	     state != GAState.RUNNING
      }) match {
         case Some(n) => state
         case None => GAState.NO_CONVERGE
      }
   }
   
   @inline
   def populationSize: Int = population.size
   
   @inline 
   def chromosomeSize: Int = if(population.size > 0) population.chromosomes(0).size else -1
   
   private[this] def converge(population: Population[T], prev: Population[T], iters: Int): GAState = {
  	  if(population == null) 
  	  	GAState.FAILED
  	  else if( population.diff(prev, 2) != None)
  	  	GAState.SUCCEED
  	  else
  	  	GAState.RUNNING
   }
}


		/**
		 * Object companion for the Solve that defines the two constructors
		 */
object GASolver {
  def apply[T <: Gene](config: GAConfig, pop: Population[T]): GASolver[T] = new GASolver[T](config, pop)
  def apply[T <: Gene](config: GAConfig, initialize: () => Population[T]): GASolver[T] = new GASolver[T](config, initialize())
}

// ---------------------------  EOF -----------------------------------------------
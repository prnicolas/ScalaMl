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
package org.scalaml.ga

import scala.util.Random
import Chromosome._
import org.scalaml.core.design.PipeOperator
import org.scalaml.core.types.ScalaMl.{DblVector, DblMatrix}
import org.scalaml.core.XTSeries
import org.apache.log4j.Logger
import org.scalaml.util.Display

sealed abstract class GAState(val description: String)
case class GA_SUCCEED(val _description: String) extends GAState(_description)
case class GA_FAILED(val _description: String) extends GAState(_description)
object GA_RUNNING extends GAState("Running")
object GA_NOT_RUNNING extends GAState("Not Running")
case class GA_NO_CONVERGENCE(val _description: String) extends GAState(_description)





	/**
	 * <p>Class to select the best solution or Chromosome from an initial population
	 * or genes pool using a set of policies for selection, mutation and crossover of
	 * chomosomes. The client code initialize the GA solver with either an initialized
	 * population or a function () => Population{T] that initialize the population. THe
	 * class has only one public method search.<br>
	 * Reference: http://www.kddresearch.org/Publications/Book-Chapters/Hs5.pdf</p>
	 * @constructor Create a generic GA-based solver. [state] Configuration parameters for the GA algorithm, [population] Initialized population of chromosomes (solution candidates)
	 * @param state Configuration parameters for the GA algorithm
	 * @param population initialized population of chromosomes (solution candidates)
	 * @throws IllegalArgumenException if the configuration is undefined or the population is not initialized
	 * 
	 * @author Patrick Nicolas
	 * @since August 29, 2013
	 * @note Scala for Machine Learning
	 * /
	 */
final class GASolver[T <: Gene](config: GAConfig, 
		                        score: Chromosome[T] =>Unit) extends PipeOperator[Population[T], Population[T]] {
   require(config != null, "GA stateuration is undefined")
   require(score != null, "Cannot search with an undefined fitness Function")
   private var state: GAState = GA_NOT_RUNNING
   
   private val logger = Logger.getLogger("GASolve")
	/**
	 * <p>Method to resolve any optimization problem using an initial genes pool 
	 * (population of Chromosomes. The minimum size of the population is 2</p>
	 * @param fitness fitness Function used to rank and select a chromosome.
	 * @throws IllegalArgumenException if the fitness function is not provided.
	 */
    
   def |>(initialize: () => Population[T]): Population[T] = this.|>(initialize())
   
   override def |> : PartialFunction[Population[T], Population[T]] = {	
  	 case population: Population[T] if(population.size > 1) => {
		  val reproduction = Reproduction[T](score)
	      state = GA_RUNNING
	          
	      Range(0, config.maxCycles).find(n => {  		 
	         reproduction.mate(population, config, n) match {
	        	case true => converge(population, n) != GA_RUNNING
	        	case false => {
	               if(population.size == 0) 
	        		 state = GA_FAILED("Mating failed") 
	        	   else 
	        		 state = GA_SUCCEED(s"Completed in $n cycles")
	        	   true
	            }
	         }
		   
	      }) match {
	         case Some(n) => population
	         case None => state = GA_NO_CONVERGENCE(s"Failed to converge"); population
	      }
  	 }
   }
   

   
   private[this] def converge(population: Population[T], cycle: Int): GAState = {
  	  if(population == null) 
  	  	 GA_FAILED(s"Failed at $cycle")
  	  else if(cycle >= config.maxCycles)
  	  	 GA_NO_CONVERGENCE(s"Failed to converge at $cycle ")
  	  else
  	   	 GA_RUNNING
   }
}


		/**
		 * Object companion for the Solve that defines the two constructors
		 */
object GASolver {
  	      
  def apply[T <: Gene](config: GAConfig, score: Chromosome[T] =>Unit): GASolver[T] = 
  	      new GASolver[T](config, score)
  	      
  def apply[T <: Gene](config: GAConfig): GASolver[T] = 
  	      new GASolver[T](config, null)
}

// ---------------------------  EOF -----------------------------------------------
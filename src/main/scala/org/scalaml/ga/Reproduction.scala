/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.ga


		/**
		 * <p>Define a replication phase in the execution of the genetic algorithm optimizer. The
		 *  A replication sequence consists of a selection of chromosomes according to their 
		 *  fitness values, cross-over within a pair of chromomsomes and mutation</p>
		 *  @param config configuration parameters of the genetic algorithm
		 *  @param f fitness function for a chromosome
		 *  @throws IllegalArgumentException if the configuration or the fitness function is undefined
		 *  		 
		 *  @author Patrick Nicolas
		 *  @since August 28, 2013
		 *  @note Scalal for Machine Learning
		 */
import Chromosome._
import scala.collection.mutable.ArrayBuffer
    import scala.util.Random  
    
final class Reproduction[T <: Gene](private val config: GAConfig, val fit: Chromosome[T] => Double) { 	   
    require(config != null, "Configuration of GA is undefined")
    require(fit != null, "The chromosome fitness function of GA is undefined")
    
    		/**
    		 * <p>Execute the 3 phases of the genetic replication: Selection, Cross-over and Mutation.</p>
    		 * @param xySet set of chromosomes contributing to the replication
    		 * @param iterationId  number of the iteration used in binding the mutation rate
    		 * @return mutable array of chromosomes if the selection succeeds, None otherwise.
    		 */    
    def mate(population: Population[T]): Option[Population[T]] = select(population) match  {
	   case Some(pop) =>  Some(pop +- config.xover ^ config.mutate)
	   case None => None   
	 }
  
    override def toString : String =new StringBuilder("Replication:\n").append(config.toString).toString

        
	private[this] def select(population: Population[T]) =  population.size match {
	   case 1 => None
	   case 2 => Some(population)
	   case _ => Some(population.select(Random.nextDouble, fit))
	}
}


object Reproduction {
   import scala.util.Random
   def apply[T <: Gene](params: GAConfig, f: Chromosome[T] => Double): Reproduction[T] = new Reproduction[T](params, f)
}

// ------------------------------  EOF --------------------------------------------------
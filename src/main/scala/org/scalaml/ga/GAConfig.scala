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

import org.scalaml.core.Design.Config


		/**
		 * <p>Configuration class that define the key parameters for the execution of the
		 * genetic algorithm optimizer. Mutation and/or Cross-over parameters may be computed using an 
		 * attenuator using the formula  newValue = oldValue*currentIteration/maximumIterations.</p>		 
		 * @constructor Create a configuration for the genetic algorithm.
		 * @throws throw IllegalArgumentException if some of the parameters are out of bounds such 
		 * as maxPopulationSize > 1 or rejection rate < 0.95
		 * @param xover Value of the cross-over parameter, in the range [0.0, 1.0] used to compute 
		 * the index of bit string representation of the chromosome for cross-over
		 * @param mutate Value in the range [0.0, 1.0] used to compute the index of the bit or 
		 * individual to be mutate in each chromosome.
		 * @param maxCycles Maximum number of iterations allowed by the genetic solver 
		 * (reproduction cycles).
		 * @param softLimit  Soft limit function (Linear, Square or Boltzman) used to attenuate 
		 * the impact of cross-over or mutation operation during optimization.</span></pre></p>
		 * @param mutation Mutation computed using an attenuation function mutation = f(cycle)
		 * @author Patrick Nicolas
		 * @since August 28, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm/implementation/GA configuration
		 */
final class GAConfig(
		val xover: Double, 
		val mu: Double, 
		val maxCycles: Int, 
		val softLimit: Int => Double) extends Config {
	import GAConfig._

	check(xover, mu, maxCycles, softLimit)
  	  
		/**
		 * <p>re-compute the mutation factor using an attenuator</p>
		 */
	val mutation = (cycle : Int) => {
		require(cycle >= 0 && cycle < maxCycles, s"GAConfig Iteration $cycle is out of range")
		softLimit(cycle)
	}
    
	override def toString : String = s"Cross-over: $xover Mutation: $mu"
}


		/**
		 * <p>Singleton that define the attenuator function for computing the cross-over or 
		 * mutation index of chromosomes, according the number of iterations in the genetic 
		 * algorithm optimization.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since August 28, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm/implementation/GA configuration
		 */
object GAConfig {
  
		/**
		 * Default constructor for the GAConfig class
		 * @param xover Value of the cross-over parameter, in the range [0.0, 1.0] used to compute 
		 * the index of bit string representation of the chromosome for cross-over
		 * @param mutate Value in the range [0.0, 1.0] used to compute the index of the bit or 
		 * individual to be mutate in each chromosome.
		 * @param maxCycles Maximum number of iterations allowed by the genetic solver 
		 * (reproduction cycles).
		 * @param softLimit  Soft limit function (Linear, Square or Boltzman) used to attenuate 
		 * the impact of cross-over or mutation operation during optimization.</span></pre></p>
		 */
	def apply(xover: Double, mu: Double, maxCycles: Int, softLimit: Int =>Double): GAConfig = 
		new GAConfig(xover, mu, maxCycles, softLimit)

		/**
		 * <p>Constructor for the GAConfig class with a default soft limit defined as<br>
		 *  f(n) = 1.001 -0.01.n.</p>
		 * @param xover Value of the cross-over parameter, in the range [0.0, 1.0] used to 
		 * compute the index of bit string representation of the chromosome for cross-over
		 * @param mutate Value in the range [0.0, 1.0] used to compute the index of the bit or 
		 * individual to be mutate in each chromosome.
		 * @param maxCycles Maximum number of iterations allowed by the genetic solver 
		 * (reproduction cycles).
		 */
	private val DEFAULT_SOFTLIMIT = (n : Int) => -0.01*n + 1.001
	def apply(xover: Double, mu: Double, maxCycles: Int): GAConfig = 
			new GAConfig(xover, mu, maxCycles, DEFAULT_SOFTLIMIT)
   
	private val DEFAULT_MAX_CYCLES = 2048
	private def check(xover: Double, mu: Double, maxCycles: Int, softLimit: Int =>Double): Unit = {
		require(maxCycles > 5 & maxCycles < DEFAULT_MAX_CYCLES, 
				s"GAConfig Maximum number of iterations $maxCycles is out of bounds [0, MAX_CYCLES]")
		require(mu > 0.0 && mu < 1.0, 
				s"GAConfig Mutation factor $mu is out of bounds [0, 1]")
		require(xover > 0.0 && xover < 1.0, 
				s"GAConfig Crossover factor $xover is out of bounds [0, 1]")
	}
}



// ----------------------------  EOF ----------------------------------------------------

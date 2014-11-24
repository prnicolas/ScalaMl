/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.ga

import org.scalaml.core.design.Config




		/**
		 * <p>Configuration class that define the key parameters for the execution of the
		 * genetic algorithm optimizer. Mutation and/or Cross-over parameters may be computed using an attenuator using the
		 * formula  newValue = oldValue*currentIteration/maximumIterations.<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
		 * <b>xover</b>         Value of the cross-over parameter, in the range [0.0, 1.0] used to compute the index of bit string representation of the chromosome for cross-over
		 * <b>mutate</b>       Value in the range [0.0, 1.0] used to compute the index of the bit or individual to be mutate in each chromosome.
		 * <b>maxCycles</b>    Maximum number of iterations allowed by the genetic solver (reproduction cycles).
		 * <b>softLimit</b>   Soft limit function (Linear, Square or Boltzman) used to attenuate the impact of cross-over or mutation operation during optimization.</span></pre></p>
		 * @constructor Create a configuration for the genetic algorithm. [maxPopulationSize]: maximum size of the population (number of chromosomes) allowed during the optimization, [xover] Value of the cross-over parameter, in the range [0,1] used to compute the index of bit string representation of the chromosome for cross-over, [mutate] Value in the range [0.25,0.95] used to compute the index of the bit or individual to be mutate in each chromosome, [maxNumIters]: Maximum number of iterations allowed in the optimization, [attenuatorType]: Type of attenuation function (Linear, Square or Boltzman) used to attenuate the impact of cross-over or mutation operation as the optimization goes along. 
		 * @throws throw IllegalArgumentException if some of the parameters are out of bounds such as maxPopulationSize > 1 or rejection rate < 0.95
		 * 	 
		 * @author Patrick Nicolas
		 * @since August 28, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm/implementation/GA configuration
		 */
final class GAConfig(val xover: Double, val mu: Double, val maxCycles: Int, val softLimit: Int => Double) extends Config {
	import GAConfig._

	check(xover, mu, maxCycles, softLimit)
	val persists = "config/ga"
  	  
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
		 * <p>Singleton that define the attenuator function for computing the cross-over or mutation index
		 * of chromosomes, according the number of iterations in the genetic algorithm optimization.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since August 28, 2013
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithm/implementation/GA configuration
		 */
object GAConfig {
	def apply(xover: Double, mu: Double, maxCycles: Int, softLimit: Int =>Double): GAConfig = 
		new GAConfig(xover, mu, maxCycles, softLimit)
   
	val DEFAULT_SOFTLIMIT = (n : Int) => -0.01*n + 1.001
	def apply(xover: Double, mu: Double, maxCycles: Int): GAConfig = 
		new GAConfig(xover, mu, maxCycles, DEFAULT_SOFTLIMIT)
   
	val MAX_CYCLES = 2048
	private def check(xover: Double, mu: Double, maxCycles: Int, softLimit: Int =>Double): Unit = {
		require(maxCycles > 5 & maxCycles < MAX_CYCLES, s"GAConfig Maximum number of iterations $maxCycles is out of bounds [0, MAX_CYCLES]")
		require(mu > 0.0 && mu < 1.0, s"GAConfig Mutation factor $mu is out of bounds [0, 1]")
		require(xover > 0.0 && xover < 1.0, s"GAConfig Crossover factor $xover is out of bounds [0, 1]")
		require(softLimit != null, "GAConfig Cannot create a configuration for a GA with undefined softlimit function")
	}
}



// ----------------------------  EOF ----------------------------------------------------

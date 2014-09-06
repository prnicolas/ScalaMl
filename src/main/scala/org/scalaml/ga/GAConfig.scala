/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.ga




	/**
	 * <p>Configuration class that define the key parameters for the execution of the
	 * genetic algorithm optimizer. Mutation and/or Cross-over parameters may be computed using an attenuator using the
	 * formula  newValue = oldValue*currentIteration/maximumIterations.</p>
	 * 
	 * @param maxPopulationSize  maximum size of the population (number of chromosomes) allowed during the optimization
	 * @param xover value of the cross-over parameter, in the range [0,1] used to compute the index of bit string representation of the chromosome for cross-over	 * @attribute rejectionRate  rate of rejection in the range [0.2,0.95] used in the select the most fit chromosomes after a selection - cross-over and mutation cycle
	 * @param mutate value in the range [0.25,0.95] used to compute the index of the bit or individual to be mutate in each chromosome.
	 * @param maxNumIters maximum number of iterations allowed in the optimization
	 * @param attenuatorType  type of attenuation (Linear, Square or Boltzman) used to attenuate the impact of cross-over or mutation operation as the optimization goes along
	 * @throws throw IllegalArgumentException if some of the parameters are out of bounds such as maxPopulationSize > 1 or rejection rate < 0.95
	 * 	 
	 * @author Patrick Nicolas
	 * @since August 28, 2013
	 * @note Scala for Machine Learning
	 */
final class GAConfig(val maxPopulationSize: Int,
                     val xover: Double,
                     val mutate: Double,
                     val maxNumIters: Int,
                     var attenuatorType: Int = GAConfig.LINEAR) {
  
  	require(maxNumIters > 5 & maxNumIters < 1000, "Maximum number of iterations is out of bounds")
  	require(maxPopulationSize > 1, "Initial chromosones population is too small")
  	require(mutate > 0.25 && mutate < 0.95)
  	
  		/**
  		 * <p>re-compute the mutation factor using an attenuator
  		 */
  	@inline
    val mutation = (iteration : Int) => {
    	require(iteration >= 0 && iteration < maxNumIters, "Iteration " + iteration + " is out of range")
        GAConfig.attenuator(attenuatorType)(iteration, maxNumIters, mutate)
    }
    
    override def toString : String = {
        new StringBuilder("Max population:" ).
                  append(maxPopulationSize).
                    append("\nCross-over: ").
                      append(xover).
                            append("\nMutation: ").
                              append(mutate).toString
    }
}


	/**
	 * <p>Singleton that define the attenuator function for computing the cross-over or mutation index
	 * of chromosomes, according the number of iterations in the genetic algorithm optimization. The
	 * attenuation function supported are linear, square and boltzman.</p>
	 * @author Patrick Nicolas
	 */
object GAConfig {
   def apply(maxPopulationSize: Int, xover: Double, mutate: Double, maxNumIters: Int): GAConfig = 
                    new GAConfig(maxPopulationSize, xover, mutate, maxNumIters)
   
    final val NONE : Int = 0
    final val LINEAR : Int = 1
    final val SQUARE : Int = 2
    final val BOLTZMAN : Int = 3
    
    private final val none = (iteration : Int, maxIters: Int, x: Double) => x
            
    private final val linear = (iteration : Int, maxIters: Int, x: Double) => { 
        x + (1.0-x)*iteration/(maxIters+1)
    }
    
    private final val boltzman = (iteration : Int, maxIters: Int, x: Double) => { 
        x + (1.0-x)*Math.exp(iteration - maxIters)
    }
    
    private final val square = (iteration : Int, maxIters: Int, x: Double) => { 
       x + (1.0-x)*iteration*iteration/(maxIters*maxIters+1)
    }
    
    type AttenuatorType = (Int, Int, Double)=>Double
    final val attenuator =Array[AttenuatorType] (none, linear, square, boltzman )

}



// ----------------------------  EOF ----------------------------------------------------

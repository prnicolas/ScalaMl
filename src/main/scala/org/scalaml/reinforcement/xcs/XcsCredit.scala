/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied..
 */
package org.scalaml.reinforcement.xcs

import org.scalaml.reinforcement.qlearning.QLState
import org.scalaml.core.Types.ScalaMl._



		/**
		 * <p>Definition of the credit to be assigned to "deserving" chromosomes
		 * and the rules it contains,  after receiving feedback from the system monitored 
		 * by the XCS algorithm.
		 * The assignment of credit consists of applying a factor ( >0) to the fitness of
		 * the chromosomes for which the rules actions improves the system (positive reward)
		 * a factor <0) otherwise.<br>
		 * The class inherits the QLState class so credit can be computed using historical
		 * data fed to the Q-Learning algorithm.</p>
		 * @param fitnesses array of fitness for the chromosomes which one or more rules have been fired
		 * @throws IllegalArgumentException if the array of the fitness of the matching chromosomes is undefined
		 * @see org.scalaml.reinforcement.qlearning.QLState
		 * 
		 * @author Patrick Nicolas
		 * @since March 25, 2014
		 * @note Scala for Machine Learning
		 */
import XcsCredit._
class XcsCredit(val fitnesses: DblVector) extends QLState[Double](-1, fitnesses, CREDIT ) {
	
		/**
		 * <p>Override the default computation of the Q-Value of this credit to be
		 * assigned to the matching chromosomes. In this simple case, the input to the 
		 * Q_Value is a simple factor to be applied to the chromosomes fitness.</p>
		 * @param  data factor (data(0)) applied to the chromosome fitness to reward (factor >0) 
		 * or punish (factor <0) the chromosomes that have been fired
		 * @throws IllegalArgumenException if the input data is undefined.
		 */
	override  def V(data: DblVector): Double =  {
	   require(data != null && data.size > 0, "Cannot compute value of this state with undefined")
	   
	   val factor = data(0)
  	   distribution.foldLeft(0.0)((s, x) => s + cost(factor, x) )
	}
}

		/**
		 * <p>Companion object that defines the constructors for the XcsCredit and
		 * the default credit computation function, CREDIT.</p>
		 * 
		 * @author Patrick Nicolas
		 */
object XcsCredit {
   final val CREDIT = (fitness: Double, value: Double) =>fitness*value
   
   def apply(fitnesses: DblVector): XcsCredit = new XcsCredit(fitnesses)
   def apply(fitnesses: List[Double]) : XcsCredit = new XcsCredit(fitnesses.toArray)
}

// ----------------------------------  EOF --------------------------------------------------------------------
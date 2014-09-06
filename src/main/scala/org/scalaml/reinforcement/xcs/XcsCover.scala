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
package org.scalaml.reinforcement.xcs

import org.scalaml.trading.Signal;
import org.scalaml.reinforcement.qlearning.QLState

import scala.util.Random

        /**
 		 * <p>Implementation of basic covering algorithm. This covering is invoked
		 * when no rule has a signal or predicate that match the input data. The process
		 * generate random, or pseudo-random rules.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since March 25, 2014
		 * @note Scala for Machine Learning
		 */
import XcsRule._
object XcsCover {  
   		/**
   		 * <p>Generates new rules from an existing population. The number of rules
   		 * to generates is the size of the chromosome representing a trading strategy.</p>
   		 * @param signal (or rule predicate) used to generate a set of new rules
   		 * @param number of rules to generated.
   		 * @throws IllegalArgumenException if the signal is undefined
   		 * @return list of new XCS rules. 
   		 */
   def create(signal: Signal, numRules: Int): List[XcsRule[Double]] = {
  	  require(signal != null, "Cannot generates new rules from undefined signal or predicate")
  	  
  	  Range(0, numRules).foldLeft(List[XcsRule[Double]]()) ( (xs, n) => {
  	  	 val action: XcsAction[Double] =  createRandomAction(rulesCount + n, 6)  // force conversion
  	  	 XcsRule[Double](signal, action) :: xs
  	  })
   } 
}

// -------------------------  EOF -----------------------------------------

/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99.1
 */
package org.scalaml.reinforcement.xcs

import org.scalaml.trading.Signal
import org.scalaml.reinforcement.qlearning.QLState
import scala.util.Random
import org.scalaml.trading.operator.SOperator
import org.scalaml.ga.Quantization
import org.scalaml.ga.Gene.Encoding


		/**
 		 * Implementation of basic covering algorithm. This covering is invoked
		 * when no rule has a signal or predicate that match the input data. The process
		 * generate random, or pseudo-random rules.
		 * 
		 * @author Patrick Nicolas
		 * @since March 25, 2014
		 * @see Scala for Machine Learning Chapter 11 Reinforcement learning / Extended learning 
		 * classifier systems
		 */
object XcsCover {  
	final val MAX_NUM_ACTIONS = 2048
		/**		 
		 * Generates new rules from an existing population. The number of rules
		 * to generates is the size of the chromosome representing a trading strategy.
		 * @param sensor (or rule predicate) used to generate a set of new rules
		 * @param actions list of actions (or XCS rules) used in the coverage process
		 * @throws IllegalArgumentException if the list of actions is undefined
		 * @return list of new XCS rules. 
		 */
	def cover(	
			sensor: XcsSensor, 
			actions: List[XcsAction])(implicit quant: Quantization, geneBits: Encoding): List[XcsRule] = {
	  
		require( actions.nonEmpty,
				"XcsCover.cover Cannot generates new rules from undefined list of actions")
		require(actions.nonEmpty && actions.size < MAX_NUM_ACTIONS,
				s"XcsCover.cover The number of actions per state ${actions.size} if out of range")
		
		actions./:(List[XcsRule]()) ((xs, act) => {
			val signal = Signal(sensor.id, sensor.value, new SOperator(Random.nextInt(Signal.numOperators)))
			new XcsRule(signal, XcsAction(act, Random)) :: xs
		})
	} 
}


// -------------------------  EOF -----------------------------------------

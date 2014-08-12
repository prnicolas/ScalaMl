/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.reinforcement.xcs

import scala.collection.mutable.HashMap

import org.scalaml.trading.Signal
import org.scalaml.ga.{Operator, Gene}
import org.scalaml.reinforcement.qlearning.QLState
import org.scalaml.core.Types.ScalaMl._


		/**
		 * <p>Class that define a rule or policy in XCS algorithm. The rule is encoded as a gene so
		 * it can be manipulated by the Genetic Algorithm. A rule is defined by the format:<br>
		 * IF signal THEN action.<br> The constructor increase a global rules count used to automatically
		 * assigned a label to each signal/predicate.</p>
		 * @param signal  signal or predicated of the rule
		 * @param action action of the rule
		 * @throws IllegalArgumenException if the predicate or the action of the rule is undefined
		 * 
		 * @author Patrick Nicolas
		 * @since March 24, 2014
		 * @note Scala for Machine Learning
		 */
import XcsRule._
class XcsRule[T <% Double](val signal: Signal, val action: XcsAction[T]) extends Gene(signal.value, signal.op)(DIGITIZE) {
   require(signal != null, "Cannot create an XCS rule with undefined signal/predicate")
   require(action != null, "Cannot create an XCS rule with undefined action")
   
   rulesCount += 1
}


			/**
			 * <p>Companion singleton for the XCS rule, The object defines the following element:<br>
			 * Default discretization method, DIGITIZE<br>
			 * XcsAction type as a Q-Learning state<br>
			 * Global rule counter, rulesCount<br>
			 * Creationof random action required by the covering process.</p>
			 * 
			 * @author Patrick Nicolas
			 * @since March 24, 2014
		     * @note Scala for Machine Learning
			 */
import Operator._
object XcsRule {
	import QLState._
	final val DIGITIZE = (x:Double) => (x*1e+4).floor.toInt
	type XcsAction[T] = QLState[T]
	
	def createRandomAction[T <% Double](id: Int, dim: Int): XcsAction[T] = createRandomState(id, dim) 
	
	var rulesCount = 0
	def apply[T <% Double](signal: Signal, action: XcsAction[T]): XcsRule[T] = new XcsRule[T](signal, action)
	def apply[T <% Double](id: String, threshold: Double, op: Operator, action: XcsAction[T]): XcsRule[T] = new XcsRule[T](Signal(id, threshold, op)(DIGITIZE), action)
}

   

// -----------------------------------------  EOF ----------------------------------------
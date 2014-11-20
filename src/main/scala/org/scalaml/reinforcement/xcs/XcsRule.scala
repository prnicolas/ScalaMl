/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.reinforcement.xcs

import scala.collection.mutable.HashMap
import org.scalaml.trading.Signal
import org.scalaml.trading.operator.EQUAL
import org.scalaml.ga.{Operator, Gene, Discretization}
import org.scalaml.reinforcement.qlearning.QLState
import org.scalaml.core.types.ScalaMl._
import XcsRule._
import org.scalaml.ga.Chromosome
import scala.util.Random



		/**
		 * <p>Class that defined a action associated to a sensor and a target value. A typical
		 * action is a sensor exceeding a target value (or threshold). XCS action are defined
		 * as gene so they can be chained as chromosomes and define a strategy to optimize the 
		 * operation of a system.<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
		 * <b>sensorId</b>    Identifier of the sensor for which an action may be triggered or fired.
		 * <b>target</b> threshold value associated to a sensor to trigger the action.
		 * </span></pre></p>
		 * @constructor Create an XCS action. 
		 * @param sensorId identifier of the sensor for which an action may be triggered or fired
		 * @param target threshold value associated to a sensor to trigger the action
		 * 
		 * @author Patrick Nicolas
		 * @since March 24, 2014
		 * @note Scala for Machine Learning Chapter 11 Reinforcement learning/Extended learning classifier systems
		 */
class XcsAction(sensorId: String, target: Double)(implicit _discr: Discretization) extends Gene(sensorId, target, EQUAL)


object XcsAction {
	val XCSACTION_SIZE = 32
	def apply(action: XcsAction, r: Random): XcsAction = (action ^ r.nextInt(XCSACTION_SIZE)).asInstanceOf[XcsAction]
}

		/**
		 * <p>Class that define a rule or policy in XCS algorithm. The rule is encoded as a gene so
		 * it can be manipulated by the Genetic Algorithm. A rule is defined by the format:<br>
		 * <i>IF signal THEN action</i>.<br> The constructor increase a global rules count used to automatically
		 * assigned a label to each signal/predicate.<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
		 * <b>signal</b>    Signal or predicated of this rule
		 * <b>action</b>    XCS action associated with this rule.
		 * @constructor Create a XCS rule as a pair of signal and action. 
		 * @throws IllegalArgumenException if the predicate or the action of the rule is undefined
		 * 
		 * @author Patrick Nicolas
		 * @since March 24, 2014
		 * @note Scala for Machine Learning Chapter 11 Reinforcement learning/Extended learning classifier systems
		 */
class XcsRule(val signal: Signal, val action: XcsAction) {
	require(signal != null, "XcsRule Cannot create an XCS rule with undefined signal/predicate")
	require(action != null, "XcsRule Cannot create an XCS rule with undefined action")
}



		/**
		 * <p>Companion singleton for the XCS rule, The object defines the XcsSensor type as 
		 * a trading signal.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since March 24, 2014
		 * @note Scala for Machine Learning Chapter 11 Reinforcement learning/Extended learning classifier systems
		*/
object XcsRule {
	type XcsSensor = Signal
}

   

// -----------------------------------------  EOF ----------------------------------------
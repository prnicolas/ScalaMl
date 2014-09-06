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
package org.scalaml.trading

import org.scalaml.ga.{Operator, Gene}
import scala.annotation.implicitNotFound


		/**
		 * <p>Define a trading signal as used in technical analysis of financial markets. A partial
		 * list of trading signals, include volume, Flow index, momentum or oscillators). This class
		 * inherit the Gene so trading signals can be threaded into trading strategy implemented
		 * as chromosomes of a genetic algorithm.<br>
		 * The class assume that a digitization function that discrete a continuous value is defined
		 * implicitly. </p>
		 * @param id  The label for the trading signal
		 * @param op The operator that is used to defined the condition such as greater than, equals....
		 * @param targetValue value threshold defined by the signal
		 * @throws IllegalArgumentException if the operator or id is not defined
		 * @see org.scalaml.ga.Gene
		 * 
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning
		 * @since Mar 4, 2014
		 */

import Operator._
import Signal._
@implicitNotFound("Signal does not have a dicretization function implicitly defined")
class Signal(val id: String, val targetValue: Double, val op: Operator)(implicit discr: Double => Int) extends Gene(targetValue, op) {
   require( id != null && id.length > 0, "Cannot create a signal with undefined id")
   require( op != null, "Cannot create a signal with undefined operator")
   
   		/**
   		 * Computation of the score of this trading signal by comparing a value with the threshold, value
   		 * @param x value to compare with the target value
   		 * @param factor amplification factor for the generation of the score
   		 * @return computed score for this trading signal
   		 */
   def score(x: Double, factor: Double = 1.0) = op match  {
      case LESS_THAN =>  (value -x)*factor
      case GREATER_THAN => (x - value)*factor
      case EQUAL => Math.abs(x - value)*factor
      case _ => 0.0
   }
   
   		/**
   		 * Compare this trading signal with another one
   		 * @param that  other trading signal
   		 * @return true if the two trading signals share the same operator and threshold value
   		 */
   final def == (that: Signal): Boolean = op == that.op && Math.abs(value - that.value) < EPS
   
   override def toString: String = {
      new StringBuilder(id)
            .append(" ")
              .append(op.toString)
                 .append(" ")
                   .append(String.valueOf(value)).toString
   }
}



	/**
	 * Companion object to the trading signal class, used to defined constructors.
	 */
object Signal {
   final val EPS = 1e-3
   final val CSV_DELIM = ",";
   def apply(id: String, targetValue: Double, op: Operator)(implicit f: Double => Int): Signal = new Signal(id, targetValue, op)
}


import Signal._
case class TradingStrategy(val fileName: String, val signals: List[Signal])

// ------------------------ EOF --------------------------------------------------------
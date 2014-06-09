/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
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
		 * @param _value value threshold defined by the signal
		 * @exception IllegalArgumentException if the operator or id is not defined
		 * @see org.scalaml.ga.Gene
		 * 
		 * @author Patrick Nicolas
		 * @project Scala for Machine Learning
		 * @date Mar 4, 2014
		 */

import Operator._
import Signal._
@implicitNotFound("Signal does not have a dicretization function implicitly defined")
class Signal(val id: String, val threshold: Double, val op: Operator)(implicit f: Double => Int) extends Gene(threshold, op) {
   require( id != null && id.length > 0, "Cannot create a signal with undefined id")
   require( op != null, "Cannot create a signal with undefined operator")
   
   		/**
   		 * Score this signal by comparing the input value with the threshold
   		 */
   def score(x: Double, delta: Double = 1.0) = op match  {
      case LESS_THAN =>  (value -x)*delta
      case GREATER_THAN => (x - value)*delta
      case EQUAL => Math.abs((x - value)*delta)
      case _ => 0.0
   }
   
   def == (that: Signal): Boolean = op == that.op && Math.abs(value - that.value) < EPS
   override def toString: String = {
      new StringBuilder(id)
            .append(" ")
              .append(op.toString)
                 .append(" ")
                   .append(String.valueOf(value)).toString
   }
}



object Signal {
   final val EPS = 1e-3
   final val CSV_DELIM = ",";
   def apply(id: String, threshold: Double, op: Operator)(implicit f: Double => Int): Signal = new Signal(id, threshold, op)
}


import Signal._
case class TradingStrategy(val fileName: String, val signals: List[Signal])

// ------------------------ EOF --------------------------------------------------------
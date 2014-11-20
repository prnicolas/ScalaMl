/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 */
package org.scalaml.trading.operator

import org.scalaml.ga.{Operator, Gene, Discretization}
import org.scalaml.core.types.ScalaMl._
import scala.annotation.implicitNotFound
import org.scalaml.trading.Signal
import scala.collection.mutable.ListBuffer
import org.scalaml.core.XTSeries
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import org.scalaml.util.Display
import scala.collection.mutable.TreeSet


		/**
		 * <p>Generic class that defines the operator of a trading signal.<br>
		 * A trading signal is emitted once a value (or data point) in a time series reaches a threshold (upward or downward movement).<br>
		 * A signal is triggers when x(n) > target value or x(n) < target value<br>
		 * The signal operator implements the <b>Operator</b> trait defined as an element of a <b>Gene</b> in a genetic algorithm. THe trading signal operators are None, >, < and ==.<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
		 * <b>id</b>  Identifier for the operator ("<", ">", ...
		 * </span></pre></p>
		 * @constructor Create an instance of an operator for a trading signal. 
		 * @see org.scalaml.ga.Operator
		 * 
		 * @author Patrick Nicolas
		 * @since March 5, 2014
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithms/GA for trading strategies/Trading operators
		 */
class SOperator(_id: Int) extends Operator {
		/**
		 * Identifier (number) for this operator
		 * * @return Number identifier
		 */
	override def id: Int = _id
   
		/**
		 * Create a new trading signal operator with a new identifier
		 * @param number identifier for the operator
		 * @return new trading signal operator
		 */
	override def apply(idx: Int): SOperator = new SOperator(idx)
	override def toString: String = id.toString
}

object NONE extends SOperator(0) { override def toString: String = "NA" }
object LESS_THAN extends SOperator(1) { override def toString: String = "<" }
object GREATER_THAN extends SOperator(2) { override def toString: String = ">" }
object EQUAL extends SOperator(3) { override def toString: String = "=" }


// ------------------------ EOF --------------------------------------------------------
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
 * Version 0.99
 */
package org.scalaml.trading.operator


import org.scalaml.ga.Operator

		/**
		 * Generic class that defines the operator of a trading signal.
		 * 
		 * A trading signal is emitted once a value (or data point) in a time series reaches a 
		 * threshold (upward or downward movement).
		 * {{{
		 * A signal is triggers when x(n) > target value or x(n) < target value
		 * }}}
		 * The signal operator implements the '''Operator''' trait defined as an element of 
		 * a '''Gene''' in a genetic algorithm. The trading signal operators are None, >, < and ==.
		 * @constructor Create an instance of an operator for a trading signal. 
		 * @see org.scalaml.ga.Operator
		 * 
		 * @author Patrick Nicolas
		 * @since March 5, 2014
		 * @note Scala for Machine Learning Chapter 10 Genetic Algorithms / GA for trading strategies
		 *  / Trading operators
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
	override def apply(idx: Int): SOperator = SOperator.SOPERATORS(idx) 
	override def toString: String = id.toString
}

	/**
	 * Definition of the None operator
	 */
object NONE extends SOperator(0) { override def toString: String = "NA" }
	/**
	 * Definition of the 'Lesser than' operator
	 */
object LESS_THAN extends SOperator(1) { override def toString: String = "<" }
	/**
	 * Definition of the 'Greater than' operator
	 */
object GREATER_THAN extends SOperator(2) { override def toString: String = ">" }

	/**
	 * Definition of the 'equal' operator
	 */
object EQUAL extends SOperator(3) { override def toString: String = "=" }


object SOperator {
	protected val SOPERATORS = Array[SOperator](NONE, LESS_THAN, GREATER_THAN, EQUAL)
}
// ------------------------ EOF --------------------------------------------------------
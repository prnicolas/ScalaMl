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
package org.scalaml.trading


import scala.annotation.implicitNotFound

import org.scalaml.ga.{Operator, Gene, Quantization}
import org.scalaml.trading.operator._
import org.scalaml.core.Types.ScalaMl.{DblVector, DblArray}
import org.scalaml.stats.XTSeries
import org.scalaml.util.DisplayUtils
import Gene._


		/**
		 * Define a trading signal as used in technical analysis of financial markets. A partial
		 * list of trading signals, include volume, Flow index, momentum or oscillators. This class
		 * inherit the '''Gene''' class so trading signals can be threaded into trading strategy 
		 * implemented as chromosomes of a genetic algorithm.
		 * 
		 * A trading signal is emitted once a value (or data point) in a time series reaches a 
		 * threshold (upward or downward movement).
		 * {{{
		 * A signal is triggers when x(n) > target value or x(n) < target value
		 * }}}
		 * The class assume that a digitization function that discrete a continuous value is defined
		 * implicitly.
		 * @constructor Create a trading signal used for analyzing changes in variables derived from 
		 * the price and trading volume of a security. 
		 * @param id Label or identifier for the trading signal
		 * @param target Target value (or threshold) used to trigger the signal.
		 * @param op Operator that is used to defined the condition such as greater than, equals.... 
		 * @param xt  Times series of single variable the signal acts upon.
		 * @param weights Weights applied to each value of the time series (optional).
		 * @param quant Quantization function that convert analog or continuous signal to a 
		 * discrete time series.
		 * 
		 * @throws IllegalArgumentException if the class parameters are not properly defined.
		 * @throws ImplicitNotFoundException if the Quantization function has not been defined.
		 * @see org.scalaml.ga.Gene
		 * 
		 * @author Patrick Nicolas
		 * @see Scala for Machine Learning Chap 10 ''Genetic Algorithm'' / GA for Trading strategies
		 * @since 0.98 March 4, 2014
		 */
final class Signal(
		id: String, 
		target: Double, 
		op: SOperator, 
		xt: DblVector, 
		weights: DblVector)(implicit quant: Quantization, encoding: Encoding)	
		  extends Gene(id, target, op) {
	import Signal._
	check(xt, weights)
 
		/**
		 * Virtual constructor used in cloning, mutation and cross-over of gene, that
		 * generate an instance of appropriate type.
		 * @param id identifier for the signal
		 * @param target Target values in the predicate/signal
		 * @param op Arithmetic or boolean operator used to trigger a signal from a value relative to 
		 * the target
		 * @return a new instance with target and operator modified through genetic reproduction but
		 * sharing the time series input xt and weights of its parent signal
		 */
	override def toGene(id: String, target: Double, op: Operator): Gene = 
			new Signal(id, target, op.asInstanceOf[SOperator], xt, weights)
	
		/**
		 * Computation of the score of this trading signal by comparing a value with the threshold, 
		 * value.
		 * @return computed score for this trading signal
		 */
	override def score: Double = 
		if( !operatorFuncMap.contains(op) ) 
			Double.MaxValue
		else 
			sumScore(operatorFuncMap.get(op).get)


		/**
		 * Compare this trading signal with another one
		 * @param that  other trading signal
		 * @return true if the two trading signals share the same operator and threshold value
		 */
	final def == (that: Signal): Boolean = op == that.op && Math.abs(target - that.target) < EPS
   
		/**
		 * Description of the trading signal using the encoded value of the target
		 * @return tuple (id, operator, encoded target value)
		 */
	override def toString: String = s"$id ${op.toString} ${String.valueOf(target)}"

	private def sumScore(f: (Double, Double) => Double): Double = 
		xt.zip(weights).map{ case(x, w) => w*f(x, target) }.sum

}



		/**
		 * Companion object to the trading signal class, used to defined constructors.
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning
		 * @since March 4, 2014 Appendix Finances 101 / Technical analysis
		 */
object Signal {
	import org.scalaml.ga.Gene.Encoding
	
	private val EPS = 1e-3
	val CSV_DELIM = ","


		/**
		 * Default constructor for Signal 
		 * @param id Label or identifier for the trading signal
		 * @param target Target value (or threshold) used to trigger the signal.
		 * @param op Operator that is used to defined the condition such as greater than, equals.... 
		 * @param xt Times series of single variable the signal acts upon.
		 * @param weights Weights applied to each value of the time series (optional).
		 * @param quant Quantization function that convert analog or continuous signal to a
		 * discrete time series.
		 */
	def apply(id: String, target: Double, op: SOperator, xt: DblVector, weights: DblVector)
			(implicit quant: Quantization, encoding: Encoding): Signal = 
		new Signal(id, target, op, xt, weights)

		/**
		 * Constructor for Signal with undefined weights and observations
		 * @param id Label or identifier for the trading signal
		 * @param target Target value (or threshold) used to trigger the signal.
		 * @param op Operator that is used to defined the condition such as greater than, equals.... 
		 * @param quant Quantization function that convert analog or continuous signal to a 
		 * discrete time series.
		 */
	def apply(id: String, target: Double, op: SOperator)
	   (implicit quant: Quantization, encoding: Encoding): Signal = 
		new Signal(id, target, op, Vector.empty[Double], Vector.empty[Double])

		/**
		 * Define the ordering of a set of trading signals using the signal id.
		 * Ordering is only used for generating unique trading strategies as
		 * unique sequence of trading signals.
		 */
	val orderedSignals = Ordering.by((signal: Signal) => signal.id)

	protected val operatorFuncMap = Map[SOperator, (Double, Double) =>Double](
		LESS_THAN -> ((x: Double, target: Double) => target - x),
		GREATER_THAN -> ((x: Double, target: Double) => x -target),
		EQUAL -> ((x: Double, target: Double) => Math.abs(x -target)),
		NONE -> ((x: Double, target: Double) => -1.0)
	)
   
	@inline
	final def numOperators = operatorFuncMap.size
   
	private val MAX_TIME_SERIES_SIZE = 10000000
	
	private def check(xt: DblVector, weights: DblVector): Unit = {
		require( xt.nonEmpty, "Signal.check Cannot create a signal with undefined time series input")
		require( xt.size < MAX_TIME_SERIES_SIZE, 
				s"Signalcheck Size of the time series input, ${xt.size} if out of range")
				

		require( weights.nonEmpty, "Signal.check Cannot create a signal with undefined weights")
		require(weights.size < MAX_TIME_SERIES_SIZE, 
					s"Signalcheck Number of weights ${weights.size} if out of range")
		require(xt.size == weights.size, 
					s"Signal The number of weights ${weights.size} is != size of data ${xt.size}")
	}
}

// ------------------------ EOF --------------------------------------------------------
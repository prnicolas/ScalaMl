/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.trading

import org.scalaml.ga.{Operator, Gene, Discretization}
import org.scalaml.trading.operator._
import org.scalaml.core.Types.ScalaMl.DblVector
import scala.annotation.implicitNotFound
import org.scalaml.core.XTSeries
import org.scalaml.util.DisplayUtils



		/**
		 * <p>Define a trading signal as used in technical analysis of financial markets. A partial
		 * list of trading signals, include volume, Flow index, momentum or oscillators. This class
		 * inherit the <b>Gene</b> class so trading signals can be threaded into trading strategy 
		 * implemented as chromosomes of a genetic algorithm.<br>
		 * A trading signal is emitted once a value (or data point) in a time series reaches a 
		 * threshold (upward or downward movement).<br>
		 * A signal is triggers when x(n) > target value or x(n) < target value<br>
		 * The class assume that a digitization function that discrete a continuous value is defined
		 * implicitly.</p>
		 * @constructor Create a trading signal used for analyzing changes in variables derived from 
		 * the price and trading volume of a security. 
		 * @param id Label or identifier for the trading signal
		 * @param target Target value (or threshold) used to trigger the signal.
		 * @param op Operator that is used to defined the condition such as greater than, equals.... 
		 * @param xt  Times series of single variable the signal acts upon.
		 * @param weights Weights applied to each value of the time series (optional).
		 * @param discr Discretization function that convert analog or continuous signal to a 
		 * discrete time series.
		 * 
		 * @throws IllegalArgumentException if the class parameters are not properily defined.
		 * @throws ImplicitNotFoundException if the discretization function has not been defined.
		 * @see org.scalaml.ga.Gene
		 * 
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning
		 * @since March 4, 2014 Appendix Finances 101 / Technical analysis
		 */
@implicitNotFound("Signal does not have a discretization function implicitly defined")
final class Signal(
		id: String, 
		target: Double, 
		op: Operator, 
		xt: DblVector, 
		weights: DblVector)(implicit discr: Discretization)	extends Gene(id, target, op) {
	import Signal._
	check(xt, weights)
   
		/**
		 * Computation of the score of this trading signal by comparing a value with the threshold, value
		 * @param x value to compare with the target value
		 * @param factor amplification factor for the generation of the score
		 * @return computed score for this trading signal
		 */
	override def score: Double = sumScore(operatorFuncMap.get(op).get)
    
		/**
		 * Compare this trading signal with another one
		 * @param that  other trading signal
		 * @return true if the two trading signals share the same operator and threshold value
		 */
	final def == (that: Signal): Boolean = op == that.op && Math.abs(target - that.target) < EPS
   
		/**
		 * <p>Description of the trading signal using the encoded value of the target</p>
		 * @return tuple (id, operator, encoded target value)
		 */
	override def toString: String = s"$id ${op.toString} ${String.valueOf(target)}"
   
	private def sumScore(f: (Double, Double) => Double): Double = 
		xt.zip(weights).foldLeft(0.0)((s, x) => s + x._2*f(x._1, target))
   
}



		/**
		 * Companion object to the trading signal class, used to defined constructors.
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning
		 * @since March 4, 2014 Appendix Finances 101 / Technical analysis
		 */
object Signal {
	private val EPS = 1e-3
	val CSV_DELIM = ",";

		/**
		 * Default constructor for Signal 
		 * @param id Label or identifier for the trading signal
		 * @param target Target value (or threshold) used to trigger the signal.
		 * @param op Operator that is used to defined the condition such as greater than, equals.... 
		 * @param xt Times series of single variable the signal acts upon.
		 * @param weights Weights applied to each value of the time series (optional).
		 * @param discr Discretization function that convert analog or continuous signal to a 
		 * discrete time series.
		 */
	def apply(id: String, target: Double, op: Operator, xt: DblVector, weights: DblVector)
			(implicit discr: Discretization): Signal = 
		new Signal(id, target, op, xt, weights)

		/**
		 * Constructor for Signal with undefined weights and observations
		 * @param id Label or identifier for the trading signal
		 * @param target Target value (or threshold) used to trigger the signal.
		 * @param op Operator that is used to defined the condition such as greater than, equals.... 
		 * @param discr Discretization function that convert analog or continuous signal to a 
		 * discrete time series.
		 */
	def apply(id: String, target: Double, op: Operator)(implicit discr: Discretization): Signal = 
		new Signal(id, target, op, Array.empty, Array.empty)

		/**
		 * Define the ordering of a signal using an anonymous function
		 */
	val orderedSignals = Ordering.by((signal: Signal) => signal.id)
   
	val operatorFuncMap = Map[Operator, (Double, Double) =>Double](
		LESS_THAN -> ((x: Double, target: Double) => target - x),
		GREATER_THAN -> ((x: Double, target: Double) => x -target),
		EQUAL -> ((x: Double, target: Double) => Math.abs(x -target)),
		NONE -> ((x: Double, target: Double) => -1.0)
	)
   
	@inline
	final def numOperators = operatorFuncMap.size
   
	private val MAX_TIME_SERIES_SIZE = 10000000
	private def check(xt: DblVector, weights: DblVector): Unit = {
		require( !xt.isEmpty, "Signal.check Cannot create a signal with undefined time series input")
		require( xt.size < MAX_TIME_SERIES_SIZE, 
				s"Signalcheck Size of the time series input, ${xt.size} if out of range")
		require( !weights.isEmpty, "Signal.check Cannot create a signal with undefined weights")
		require(weights.size < MAX_TIME_SERIES_SIZE, 
				s"Signalcheck Number of weights ${weights.size} if out of range")
		require(xt.size == weights.size, 
				s"Signal The number of weights ${xt.size} is != size of data ${xt.size}")
	}
}

// ------------------------ EOF --------------------------------------------------------
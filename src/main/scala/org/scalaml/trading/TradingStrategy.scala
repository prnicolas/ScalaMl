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
import org.scalaml.core.Types.ScalaMl._
import Signal._
import scala.collection.mutable.ListBuffer
import org.scalaml.core.XTSeries

import org.scalaml.util.DisplayUtils
import scala.collection.mutable.TreeSet


		/**
		 * <p>Trading Strategy defined as a list of trading signals. The signals are linked through
		 * OR boolean operator IF( signal1 == true OR signal2 == true OR ...</p>
		 * @constructor Create an instance of a trading strategy 
		 * @throws IllegalArgumenException if the list of signals is either undefined or empty
		 * @param name Identifier or name of the strategy
		 * @param signals List or sequence of trading signals used in this strategy.
		 * 
		 * @author Patrick Nicolas
		 * @since May 7, 2014
		 * @note Scale for Machine Learning Appendix/Finances 101
		 */
case class TradingStrategy(val name: String ="", signals: List[Signal]) {
	require( !signals.isEmpty, s"TradingStrategy The list of signals is undefined")
}


		/**
		 * <p>Factory for trading strategies. The factory collects all the trading signals needed 
		 * to implement the trading strategy. The strategies are generated as the list of all 
		 * combination of nSignals trading signals, once and only once when requested. The Factory 
		 * is mainly used for initializing the population for the genetic algorithm or the
		 * extended learning classifiers.</p>
		 * @constructor Instantiate a factory for all the combination of nSignals trading signals.
		 * @throws IllegalArgumentException if the number of signals is less than 1
		 * @param nSignals Number of trading signals used in any trading strategy.
		 * @param discr Discretization function to convert signal to discrete value and vice versa
		 * 
		 * @author Patrick Nicolas
		 * @since May 7, 2014
		 * @note Scala for Machine Learning Chapter 10: Genetic Algorithms 
		 */
class StrategyFactory(nSignals: Int) (implicit discr: Discretization){
	import org.scalaml.ga.Chromosome
	import Chromosome._

	require(nSignals > 0, s"StrategyFactory Number of signals $nSignals should be >0")
   
	private[this] val signals  = new ListBuffer[Signal]
   
		/**
		 * <p>Create and add a new signal to the pool of this factory. The signal is defined by 
		 * its identifier, id, target vablue, operator, the observations its acts upon and optionally the weights
		 * @param id Identifier for the signal created and collected
		 * @param target target value (or threshold) for the signal created and collected
		 * @param op Operator of type SOperator of the signal added to the pool
		 * @param obs Observations or scalar time series used by the signal added to the pool
		 * @param weights weights for the observations used by the signal (optional)
		 */
	def += (id: String, target: Double, op: Operator, obs: DblVector, weights: DblVector): Unit = {
	  	checkArguments(obs, weights)
		signals.append(Signal(id, target, op, obs, weights) )
	}
	
		/**
		 * <p>Create and add a new signal to the pool of this factory. The signal is defined by  its identifier, id,
		 * target value, operator, the observations its acts upon and optionally the weights.</p>
		 * @param id Identifier for the signal created and collected
		 * @param target target value (or threshold) for the signal created and collected
		 * @param op Operator of type SOperator of the signal added to the pool
		 * @param xt Scalar time series used by the signal added to the pool
		 * @param weights weights for the observations used by the signal (optional)
		 */
	def += (id: String, target: Double, op: Operator, xt: XTSeries[Double], weights: DblVector): Unit = {
		checkArguments(xt.toArray, weights)
		signals.append(Signal(id, target, op, xt.toArray,weights) )
	}


		/**
		 * <p>Generates the trading strategies as any unique combinations of <b>nSignals</b>
		 * of signals currently in the pool of signals. The list of strategies is computed on demand
		 * only once
		 * @return strategies extracted from the pool of signals.
		 */
	lazy val strategies: Pool[Signal] = {
		implicit val ordered = Signal.orderedSignals
        
		val xss = new Pool[Signal]
		val treeSet = new TreeSet[Signal] ++= signals.toList
		val subsetsIterator = treeSet.subsets(nSignals)

		while( subsetsIterator.hasNext) {
			val subset = subsetsIterator.next
			val signalList: List[Signal] = subset.toList
			xss.append(Chromosome[Signal](signalList))
		}
		xss
	}
	
	private def checkArguments(xt: DblVector, weights: DblVector): Unit = {
		require( !xt.isEmpty, 
				"StrategyFactory.checkArgument Input to this trading strategy is undefined")
		require( !weights.isEmpty, 
				"StrategyFactory.checkArgument Input to this trading strategy is undefined")
	}
}


// ------------------------ EOF --------------------------------------------------------
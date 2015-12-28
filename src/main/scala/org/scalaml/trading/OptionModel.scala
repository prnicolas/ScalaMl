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
package org.scalaml.trading

import scala.collection._

import org.apache.log4j.Logger

import org.scalaml.reinforcement.qlearning
import org.scalaml.workflow.data.DataSource
import org.scalaml.stats.XTSeries
import org.scalaml.core.Types.ScalaMl.{DblVector, DblArray}
import org.scalaml.util.DisplayUtils
import org.scalaml.util.MapUtils.NumericAccumulator
import YahooFinancials._, qlearning._, XTSeries._


		/**
		 * Class that defines the model for a traded option on a security. An option is fully
		 * defined with the symbol of the underlying security, its strike price, the source of
		 * data (CSV format), the minimum expiration time left in the option and the number of
		 * steps used in the approximation of the value of the option (Discretization).
		 * @constructor Create a model for a option on a specific security
		 * @throws IllegalArgumentException if the strikeprice, minimum expiration or number of
		 * steps are out of bounds
		 * @param symbol Ticker symbol for the underlying security
		 * @param strikePrice Strike price for the option
		 * @param src Source of the historical data on the underlying security
		 * @param minExpT Minimum time before expiration of the option to be used in the analysis
		 * @param nSteps steps used in the discretization or approximation of the value of the security
		 * 
		 * @author Patrick Nicolas
		 * @since May 11, 2014 
		 * @note Scala for Machine Learning / Appendix / Finance 101 /Option Trading
		 */
final class OptionModel(
		symbol: String, 
		strikePrice: Double, 
		src: DataSource, 
		minExpT: Int, 
		nSteps: Int) {

	check(strikePrice,minExpT,nSteps)

	val propsList = (for {
		price <- src.get(adjClose)
		volatility <- src.get(volatility)
		nVolatility <- normalize(volatility)
		vltyByVol <- src.get(volatilityByVol)
		nVltyByVol <- normalize(vltyByVol)
		priceToStrike <- normalize(price.map(p => (1.0 - strikePrice/p)))
	} 
	yield {
		
		// Assemble the OptionProperties and compute the normalize minimum
		// minimum time to the expiration of the option.
	  
		nVolatility.zipWithIndex
						./:(List[OptionProperty]()){ case (xs, (v, n)) => {
			val normDecay = (n + minExpT).toDouble/(price.size + minExpT)
			new OptionProperty(normDecay, v, nVltyByVol(n), priceToStrike(n)) :: xs
		}}
		.drop(2).reverse
	})
	.getOrElse(List.empty[OptionProperty])

		/**
		 * Compute an approximation of the value of the options by 
		 * discretization the actual value in multiple levels
		 * @param y Array of option prices
		 * @return A map of array of levels for the option price and accuracy
		 */
	def quantize(o: DblArray): Map[Array[Int], Double] = {
		val mapper = new mutable.HashMap[Int, Array[Int]]
  	    
	//	val acc = new NumericAccumulator[Int]
		val acc: NumericAccumulator[Int] = propsList.view.map( _.toArray)
				.map( toArrayInt( _ ))
				.map(ar => { 
					val enc = encode(ar)
					mapper.put(enc, ar)
					enc
				})
				.zip(o)
				./:(new NumericAccumulator[Int]){ case (acc, (t, y)) => {
						acc += (t, y)
						acc }
				}
		
		acc.map{ case (k, (v, w)) => (k, v/w) }
				.map{ case( k,v) => (mapper(k), v) }.toMap
	}
   
  
	private def encode(arr: Array[Int]): Int = 
		arr./:((1, 0)){ case ((s,t), n) => (s*nSteps, t + s*n)}._2

  
	private def toArrayInt(feature: DblArray): Array[Int] = feature.map(x => (nSteps*x).floor.toInt)
	
	private def check(strikePrice: Double, minExpT: Int, nSteps: Int) : Unit = {
		require(strikePrice > 0.0, s"OptionModel.check price found $strikePrice required > 0")
		require(minExpT > 2 && minExpT < 16, 
				s"OptionModel.check Minimum expiration time found $minExpT required ]2, 16[")
		require(nSteps > 1, 
				s"OptionModel.check, number of steps found $nSteps required > 1")
	}
}


		/**
		 * Class that defines the property of a  traded option on a security.
		 * @constructor Create the property for an option.
		 * @throws IllegalArgumentException if any of the class parameters is undefined
		 * @param timeToExp Time left to the option before expiration as percentage of the overall
		 * duration of the option
		 * @param relVolatility normalized relative volatility of the underlying security for a given
		 * trading session.
		 * @param volatilityByVol Volatility of the underlying security for a given trading session 
		 * relative to a trading volume for the session
		 * @param relPriceToStrike Price of the underlying security relative to the Strike price for 
		 * a given trading session.
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98.2 May 11, 2014 
		 * @version 0.98.2
		 * @see Scala for Machine Learning Appendix "Finance 101" Option Trading
		 */
@throws(classOf[IllegalArgumentException])
class OptionProperty(
		timeToExp: Double, 
		volatility: Double, 
		vltyByVol: Double, 
		priceToStrike: Double) {
	val toArray = Array[Double](timeToExp, volatility, vltyByVol, priceToStrike)
   
	require(timeToExp > 0.01, s"OptionProperty time to expiration found $timeToExp erquierd 0.01")
}


// ------------------------------------  EOF ----------------------------------
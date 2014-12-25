/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.trading

import scala.collection._

import org.apache.log4j.Logger

import org.scalaml.reinforcement.qlearning
import org.scalaml.workflow.data.DataSource
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.util.DisplayUtils
import org.scalaml.util.MapUtils.NumericAccumulator


		/**
		 * <p>Class that defines the model for a traded option on a security. An option is fully
		 * defined with the symbol of the underlying security, its strike price, the source of
		 * data (CSV format), the minimum expiration time left in the option and the number of
		 * steps used in the approximation of the value of the option (Discretization).</p>
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
	import YahooFinancials._, qlearning._
	
	check(strikePrice,minExpT,nSteps)
		// Adjusted closing price for a security, extracted from Yahoo financials
	val price = src |> adjClose

		// extract the future price and the list of stock technical parameters
	val futurePrice = price.drop(2)
	
		// Retrieve the list of stock technical parameters or properties
	val propsList = (for {
		rVolatility <- XTSeries.normalize((src |> relVolatility).toArray)
		volByVol <- XTSeries.normalize((src |> volatilityByVol).toArray)
		relPriceToStrike <- XTSeries.normalize(price.map(p => 1.0 - strikePrice/p))
	} yield {
		
		// Assemble the OptionProperties and compute the normalize minimum
		// minimum time to the expiration of the option.
		rVolatility.zipWithIndex
						.foldLeft(List[OptionProperty]())((xs, e) => {
			val normDecay = (e._2+minExpT).toDouble/(price.size+minExpT)
			new OptionProperty(normDecay, e._1, volByVol(e._2), relPriceToStrike(e._2)) :: xs
		}).drop(2).reverse
	}).getOrElse(List.empty)

		/**
		 * Compute an approximation of the value of the options by 
		 * discretization the actual value in multiple levels
		 * @param y Array of option prices
		 * @return A map of array of levels for the option price and accuracy
		 */
	def approximate(o: DblVector): Map[Array[Int], Double] = {
		val mapper = new mutable.HashMap[Int, Array[Int]]
  	    
		val acc = new NumericAccumulator[Int]
		propsList.map( _.toArray)
				.map( toArrayInt( _ ))
				.map(ar => { val enc = encode(ar); mapper.put(enc, ar); enc})
				.zip(o)
				.foldLeft(acc)((acc, t) => {acc += (t._1, t._2); acc })
  	  
		acc.map(kv => (kv._1, kv._2._2/kv._2._1))
				.map(kv => (mapper(kv._1), kv._2)).toMap
	}
   
  
	private def encode(arr: Array[Int]): Int = 
		arr.foldLeft((1, 0))((s, n) => {
			val np = s._1*n
			(s._1*nSteps, s._2 + np)
		})._2

    	  
	private def toArrayInt(feature: DblVector): Array[Int] = feature.map(x => (nSteps*x).floor.toInt)
	
	private def check(strikePrice: Double, minExpT: Int, nSteps: Int) : Unit = {
		require(strikePrice > 0.0, s"OptionModel.check strike price $strikePrice is out of bounds")
		require(minExpT > 2 && minExpT < 16, 
				s"OptionModel.check Minimum expiration time, $minExpT is out of bounds")
		require(nSteps > 1, 
				s"OptionModel.check, number of value approximation steps $nSteps is out of bounds")
	}
}


		/**
		 * <p>Class that defines the property of a  traded option on a security.</p>
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
		 * @since May 11, 2014 
		 * @note Scala for Machine Learning / Appendix / Finance 101 /Option Trading
		 */
class OptionProperty(
		timeToExp: Double, 
		relVolatility: Double, 
		volatilityByVol: Double, 
		relPriceToStrike: Double) {
	val toArray = Array[Double](timeToExp, relVolatility, volatilityByVol, relPriceToStrike)
   
	require(timeToExp > 0.01, s"OptionProperty time to option expiration $timeToExp if out of bounds")
}

// ------------------------------------  EOF ----------------------------------
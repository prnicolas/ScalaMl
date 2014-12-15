/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.3
 */
package org.scalaml.app.chap11

import scala.collection._

import org.apache.log4j.Logger

import org.scalaml.reinforcement.qlearning
import org.scalaml.plots.{ScatterPlot, LinePlot, LightPlotTheme}
import org.scalaml.workflow.data.DataSource
import org.scalaml.core.XTSeries
import org.scalaml.trading.YahooFinancials
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.util.DisplayUtils
import org.scalaml.util.MapUtils.NumericAccumulator


class OptionModel(symbol: String, strikePrice: Double, src: DataSource, minExpT: Int, nSteps: Int) {
	import YahooFinancials._, qlearning._
	val price = src |> adjClose

	val futurePrice = price.drop(2)
	val propsList: List[OptionProperty] = {
		val rVolatility = XTSeries.normalize((src |> relVolatility).toArray).get
		val volByVol = XTSeries.normalize((src |> volatilityByVol).toArray).get
		val relPriceToStrike = XTSeries.normalize(price.map(p => 1.0 - strikePrice/p)).get

		rVolatility.zipWithIndex
					.foldLeft(List[OptionProperty]())((xs, e) => {
			val normDecay = (e._2+minExpT).toDouble/(price.size+minExpT)
			new OptionProperty(normDecay, e._1, volByVol(e._2), relPriceToStrike(e._2)) :: xs
		}).drop(2).reverse
	}
   	
	def approximate(y: DblVector): Map[Array[Int], Double] = {
		val mapper = new mutable.HashMap[Int, Array[Int]]
  	    
		val acc = new NumericAccumulator[Int]
		propsList.map( _.toArray)
				.map( toArrayInt( _ ))
				.map(ar => { val enc = encode(ar); mapper.put(enc, ar); enc})
				.zip(y)
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
}


class OptionProperty(
		timeToExp: Double, 
		relVolatility: Double, 
		volatilityByVol: Double, 
		relPriceToStrike: Double) {
   val toArray = Array[Double](timeToExp, relVolatility, volatilityByVol, relPriceToStrike)
}


// ------------------------------------  EOF ----------------------------------
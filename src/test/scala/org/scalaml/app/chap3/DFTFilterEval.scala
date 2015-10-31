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
package org.scalaml.app.chap3

import scala.util.Try
import org.apache.log4j.Logger

import org.scalaml.core.Types.ScalaMl.{DblArray, DblVector}
import org.scalaml.stats.XTSeries
import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import org.scalaml.util.{FormatUtils, DisplayUtils, LoggingUtils}
import org.scalaml.filtering.dft.{DFTFilter, DFT, DTransform}
import org.scalaml.app.Eval
import LoggingUtils._, DisplayUtils._, YahooFinancials._, FormatUtils._


		/**
		 * '''Purpose''': Evaluate the low pass filter based on the Discrete Fourier sine and
		 * cosine series. The test evaluates the impact of the cutoff frequency value on the
		 * filtering and residual noise
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98 (February 13, 2015)
		 * @version 0.99
		 * @see org.scalaml.filtering
		 * @see Scala for Machine Learning Chapter 3 ''Data pre-processing'' /Discrete Fourier transform
		 */
object DFTFilterEval extends FilteringEval {	
	import DTransform.sinc
	import scala.language.postfixOps
	import YahooFinancials._

		/**
		 * Name of the evaluation 
		 */
	val name: String = "DFTFilterEval"  
	  
	private val RESOURCE_PATH = "resources/data/chap3/"
	private val OUTPUT = "output/chap3/filt_"
	private val DISPLAY_SIZE = 128
	private val CUTOFF = 0.005
	private val CUTOFF2 = 0.01
	
		 /** 
		 * Exceptions thrown during the execution of the tests are caught by the wrapper or handler
		 * test method in Eval trait defined as follows:
		 * {{{
		 *    def test(args: Array[String]) =
		 *      Try(run(args)) match {
		 *        case Success(n) => ...
		 *        case Failure(e) => ...
		 * }}}
		 * The tests can be executed through ''sbt run'' or individually by calling 
		 * ''TestName.test(args)'' (i.e. DKalmanEval.test(Array[String]("IBM") )
		 * @param args arguments used in the test
		 */
	override protected def run(args: Array[String]): Int = {
		val symbol = args(0)
		
		show(s"$header Low pass filter using Fourier series $symbol")

		val src = DataSource(s"$RESOURCE_PATH$symbol.csv", false, true, 1)
		
			// Generate the partial function for the low pass filter
		val pfnDFTfilter = DFTFilter[Double](CUTOFF)(sinc) |>
		val pfnDFTfilter2 = DFTFilter[Double](CUTOFF2)(sinc) |>
		
			// Extract the daily adjusted stock closing price 
			// then applies the DFT filter
		(for {
			price <- src.get(adjClose)
			
				// Applies the first DFT filter with  CUTOFF = 0.005
			if(pfnDFTfilter.isDefinedAt(price))
				filtered <- pfnDFTfilter(price)
				
				// Applies the second DFT filter with  CUTOFF = 0.01
			if(pfnDFTfilter2.isDefinedAt(price))
				filtered2 <- pfnDFTfilter2(price)
		} 
		yield {
				// Store filtered data in output file
			val sink2 = DataSink[Double](s"${OUTPUT}$symbol.csv")
			sink2 |> filtered :: List[DblVector]()

				// Display the low pass filter with CUTOFF
			display(price, filtered, CUTOFF)
			
				// Display the low pass filter with CUTOFF2
			display(price, filtered2, CUTOFF2)
			val result = format(filtered.take(DISPLAY_SIZE), "DTF filtered", SHORT)
			show(s"First $DISPLAY_SIZE frequencies: $result")
		}).get
	}

	
		/*
		 * Display two time series of frequencies with a predefined cutOff value fc
		 * using the LinePlot class
		 */
	private def display(x1: DblVector, x2: DblVector, fc: Double): Unit =   {
		import org.scalaml.plots.{LinePlot, LightPlotTheme, BlackPlotTheme, Legend}
		
		var labels = Legend( name, 
				"Discrete Fourier low pass filter", 
				s"Low pass frequency $fc", 
				"Stock price")
		val _x2 = x2.take(x1.size -96)
		val data = (x1, "Stock price") :: (_x2, "DFT") :: List[(DblVector, String)]()
		LinePlot.display(data, labels, new LightPlotTheme)
		
		labels = Legend( name, 
				"Discrete Fourier low pass filter - Noise", 
				s"Low pass frequency $fc", 
				"delta price")
		val data2 = x1.zip(_x2).map{ case(x, y) => x - y }
		LinePlot.display(data2, labels, new LightPlotTheme)
	}
}

// --------------------------------------  EOF -------------------------------
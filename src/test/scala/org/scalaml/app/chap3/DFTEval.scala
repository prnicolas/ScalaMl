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
import org.scalaml.util.{FormatUtils, LoggingUtils}
import org.scalaml.filtering.dft.{DFTFilter, DFT, DTransform}
import org.scalaml.app.Eval
import LoggingUtils._, FormatUtils._



		/**
		 * '''Purpose''': Evaluate the Discrete Sine and Cosine Fourier transform.
		 * @author Patrick Nicolas
		 * @since 0.98.1 (February 22, 2014)
		 * @version 0.99
		 * @see org.scalaml.filtering
		 * @see Scala for Machine Learning  Chapter 3 ''Data pre-processing'' Discrete Fourier transform
		 */
object DFTEval extends FilteringEval {	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "DFTEval"  
	  
	  
	private val RESOURCE_PATH = "resources/data/chap3/"
	private val OUTPUT1 = "output/chap3/simulated.csv"
	private val OUTPUT2 = "output/chap3/smoothed.csv"
	private val OUTPUT3 = "output/chap3/filt_"
	private val FREQ_SIZE = 1025
	private val DISPLAY_SIZE = 128
	  
	private val F = Array[Double](0.005, 0.05, 0.2)
	private val A = Array[Double](2.0, 1.0, 0.5)

	
	private def harmonic(x: Double, n: Int): Double =  A(n)*Math.cos(Math.PI*F(n)*x)
	private val h = (x:Double) => 
			Range(0, A.size).aggregate(0.0)((s, i) => s + harmonic(x, i), _ + _ )

		/**
		 * Execution of the scalatest for '''DFT''' class 
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate
		 * 
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
		 * @param args array of arguments used in the test
		 */
	override protected def run(args: Array[String]): Int = {
		import scala.language.postfixOps
    
		show(s"$header Discrete Fourier series with synthetic data")

		
		val INV_FREQ = 1.0/FREQ_SIZE
		
			// Extract the frequencies spectrum using the discrete Fourier transform
			// and write into output file	
		val pfnDFT = DFT[Double] |>
 
			// A simple pipeline for generating harmonics, store the output data into a sink file
			// build the frequency spectrum and save it into file.
		(for {
			values <- Try{ Vector.tabulate(FREQ_SIZE)(n => h(n*INV_FREQ)) }
			output1 <- DataSink[Double](OUTPUT1) write values
			
			if( pfnDFT.isDefinedAt(values))
				frequencies <- pfnDFT(values)
			output2 <- DataSink[Double](OUTPUT2) write frequencies
		} 
		yield {
		
				// Display the 2nd to 128th frequencies
			val displayed = frequencies.take(DISPLAY_SIZE)
			display(displayed.drop(1))
			
			val results = format(displayed, "x/1025", SHORT)
			show(s"$DISPLAY_SIZE frequencies: ${results}")
		}).getOrElse(-1)
	}
			
	private def display(data: DblVector): Unit = {
	  import org.scalaml.plots.{Legend, LinePlot, LightPlotTheme}
	  
		val title = s"Discrete Fourier Frequencies 1 - ${DISPLAY_SIZE}"
		val labels = Legend(name, title, "Frequencies", "Amplitude")
		LinePlot.display(data, labels, new LightPlotTheme)
	}
}


// --------------------------------------  EOF -------------------------------
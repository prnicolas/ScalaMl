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
package org.scalaml.filtering.dft

	// Scala standard library
import scala.annotation.implicitNotFound
import scala.util.{Try, Success, Failure}

	// 3rd party libraries
import org.apache.commons.math3.transform._
import org.apache.log4j.Logger

	// ScalaMl classes
import org.scalaml.util.DisplayUtils
import org.scalaml.stats.XTSeries
import org.scalaml.core.ETransform
import org.scalaml.core.Types.ScalaMl
import DFT._, ScalaMl._, DFTFilter._

/**
 * Define the configuration of a low-pass filter based on discrete Fourier series
 * @param fC  Frequency for the low-pass filter
 * @param eps convergence/tolerance for evaluating frequencies
 */
case class DFTFilterConfig(fC: Double, eps: Double)

		/**
		 * Low-band filter based of the Discrete Fourier transform. The overloaded Pipe Operator 
		 * implements the convolution of the filter function and the input time series class.
		 * The class uses the Apache Commons Math library.
		 * @constructor Create a low-pass filter using the discrete Fourier transform
		 * @tparam T type (view bounded to Double) of the elements of the observations of the 
		 * time series
		 * @param g   Filtering function y = g(x, fC)used in the convolution
		 * @param fC  Frequency cutoff for this low pass filter.
		 * @param eps Accuracy criteria for the filter
		 * @throws IllegalArgumentException if the cut-off value is out of [0,1] range or eps is 
		 * out of range
		 * 
		 * @author Patrick Nicolas
		 * @since  0.98 February 9, 2014
		 * @version 0.99.1
		 * @see Scala for Machine Learning  Chapter 3 "Data pre-processing"  Fourier analysis / 
		 * DFT-based filtering
		 * @note The class uses the Apache Commons Math library 3.5
		 */
@throws(classOf[IllegalArgumentException])
final protected[scalaml] class DFTFilter[T <: AnyVal](
		fC: Double,
		eps: Double)(g: F2)(implicit f: T => Double)
	extends DFT[T](eps) {

	require(eps > 1e-20 && eps < 0.1, s"DFTFir found eps = $eps required > 1e+20 and < 0.1")	
	require(fC > 0.0 && fC < 1.0, s"DFTFir found fC = $fC required > 0.0 and < 1.0")
   
	private val logger = Logger.getLogger("DFTFir")
   
		/**
		 * Overload the pipe operator to compute the Discrete Fourier Cosine or Sine transform 
		 * (time series of frequency values). The type of transform is define at run-time the first 
		 * value is close to zero (Sine transform) or not (Cosine transform).
		 * @throws MatchError if the input time series is undefined
		 * @return PartialFunction of time series of elements of type T as input to the Discrete 
		 * Fourier filter and time series of frequencies of type Double as output
		 */
	override def |> : PartialFunction[U, Try[V]] = {
		case xt: U if xt.size >= 2 =>
			import Config._
	
				// Forward computation of the discrete Fourier series
			fwrd(xt).map{ case(trf, freq) =>
				// Computes the frequencies cut-off in data points
				val cutOff = fC*freq.length
						
				// Applies the cutoff to the original frequencies spectrum
				val filtered = freq.zipWithIndex.map{ case(x, n) => x*g(n, cutOff) }
					
						// Reconstruct the signal.
				trf.transform(filtered, INVERSE).toVector
		}
	}  
}


		/**
		 * Singleton the wraps the constructors for the '''DFTFir'''
		 * @author Patrick Nicolas
		 */
object DFTFilter {
	type F2 = Function2[Double, Double, Double]
  
	def apply[T <: AnyVal](fC: Double, eps: Double)(g: F2)(implicit f: T => Double): DFTFilter[T] =
		new DFTFilter[T](fC, eps)(g)

	final val EPS: Double = 1e-3
	def apply[T <: AnyVal](
			fC: Double)
			(g: F2)
			(implicit f: T => Double): DFTFilter[T] = apply(fC, EPS)(g)
}


// -----------------------  EOF --------------------------------------
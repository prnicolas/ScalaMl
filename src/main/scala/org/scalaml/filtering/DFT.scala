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
package org.scalaml.filtering

	// Scala standard library
import scala.annotation.implicitNotFound
import scala.util.{Try, Success, Failure}

	// 3rd party libaries
import org.apache.commons.math3.transform._
import org.apache.log4j.Logger

	// ScalaMl classes
import org.scalaml.util.DisplayUtils
import org.scalaml.stats.XTSeries
import org.scalaml.core.ETransform
import org.scalaml.core.Types.ScalaMl
import DFT._, ScalaMl._

		/**
		 * Generic Data transformation trait for the Fast Discrete Fourier. The main
		 * function of the trait is to pad time series to resize the original data set
		 * to the next power to 2. The internal method assumes that the implicit conversion
		 * from T to Double is defined.
		 * @tparam T type of features or observations of the time series
		 * @constructor Create a generic Fourier transform as a data transformation
		 * @see org.scala.commons.math3.transform
		 * @author Patrick Nicolas
		 * @since February 9, 2014 v 0.98.1
		 * @version 0.98.2
		 * @see Scala for Machine Learning Chapter 3 "Data pre-processing" Discrete Fourier transform
		 */
trait DTransform {
  
		/**
		 * Define the Apache Commons Math configuration parameters for the
		 * family of Discrete Fourier transform and series 
		 */
	object Config {
		final val FORWARD = TransformType.FORWARD
		final val INVERSE = TransformType.INVERSE
		final val SINE = DstNormalization.STANDARD_DST_I
		final val COSINE = DctNormalization.STANDARD_DCT_I
  }

	private[this] def padSize(xtSz: Int, even: Boolean= true): Int = {
		require(xtSz > 0, s"DTransform.padSize Cannot pad a series of size $xtSz")

			// Compute the size-boundary for the padding
		val sz = if( even ) xtSz else xtSz-1
		if( (sz & (sz-1)) == 0)  
			0
			// Compute size of the padding for the DFT 
			// by extracting the related bit position
		else {
			var bitPos = 0
			do {
				bitPos += 1
			} while( (sz >> bitPos) > 0)
				
				// Add 1 slot for  padding to the next power of two
			(if(even) (1<<bitPos) else (1<<bitPos)+1) - xtSz
		}
	}

		/**
		 * Pads the input time series with zero values to reach the next boundary of 2 at power of N.
		 * 
		 * The input parameters are validated by the client code.
		 * @param xt input time series to filter
		 * @param even flag that specifies if the boundary for the data is an even number
		 * @param f implicit conversion of type T to Double
		 * @throws ImplicitNotFoundException if the implicit conversion T => Double is undefined
		 * @return New input array padded for the DFT
		 */
	@implicitNotFound(msg = "DTransform.pad Conversion to Double is required")
	protected def pad(vec: DblVector, even: Boolean = true): DblVector = {
		val newSize = padSize(vec.size, even)
		val arr: DblVector = vec.map(_.toDouble)
		
			// Fill up the remaining array with 0.0 if the size of the 
			// padding exceeds the size of the time series.
		if( newSize > 0) arr ++ Vector.fill(newSize)(0.0) else arr
	}
}


		/**
		 * Companion object to the class DTransform that define the '''sinc''' and '''sinc2'''
		 * functions.
		 * @author Patrick Nicolas
		 * @since 0.98.1 February 9, 2014 
		 * @version 0.98.1
		 * @see Scala for Machine Learning Chapter 3 "Data pre-processing" / Discrete Fourier transform
		 */
object DTransform { 
  
  		/**
		 * Definition of the generic convolution function used in discrete Fourier transform based 
		 * low pass filters
		 * @param f frequency 
		 * @param fC frequency cutoff of this low pass filter
		 * @param t function that defined the condition for the convolution
		 * @return 1 if frequency is below cutoff, 0 otherwise
		 */
	val convol = (n: Int, f: Double, fC: Double) => 
		if( Math.abs( Math.pow(f, n)) < fC) 1.0 else 0.0
         

    	
		/**
		 * Definition of the '''sinc''' convolution function used in discrete Fourier transform based 
		 * low pass filters
		 * @param f frequency 
		 * @param fC frequency cutoff of this low pass filter
		 * @return 1 if frequency is below cutoff, 0 otherwise
		 */
   
	val sinc = convol(1, _: Double, _:Double)
		
		/**
		 * Definition of the '''sinc2''' convolution function used in discrete Fourier transform based 
		 * low pass filters
		 * @param f frequency 
		 * @param fC frequency cutoff of this low pass filter
		 * @return 1 if frequency is below cutoff, 0 otherwise
		 */
	val sinc2 = convol(2, _: Double, _:Double)

     
	val sinc4 = convol(4, _: Double, _: Double)
}

		/**
		 * Discrete Fourier Transform for time series of element of type T bounded to a Double.
		 * This class inherit the generic DTransform to access the padding functions. The transformation
		 * uses an explicit configuration, eps and therefore implement '''ETransform'''
		 * @tparam T type (view bounded to Double) of the elements of the observations of the 
		 * time series
		 * @constructor Create a Discrete Fourier transform series
		 * @param eps maximum value of the first observation to use a fast sine series for the 
		 * discrete Fourier transform
		 * @author Patrick Nicolas
		 * @since 0.98  Feb 24, 2014
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 3 "Data pre-processing" Discrete Fourier transform
		 */
@implicitNotFound(msg = "DFT Conversion from $T to Double is required")
protected class DFT[T](eps: Double)(implicit c: T => Double) 
		extends ETransform[Double](eps) with DTransform {
	private val logger = Logger.getLogger("DFT")
	
	type U = XSeries[T]
	type V = DblVector
  
		/**
		 * Overload the pipe operator to compute the Discrete Fourier Cosine or Sine transform 
		 * (time series of frequency values). The type of transform is define at run-time the first 
		 * value is close to zero (Sine transform) or not (Cosine transform).
		 * @throws MatchError if the input time series is undefined or its size is less or equal to 2
		 * @return PartialFunction of type vector of Double for input to the Discrete Fourier Transform, 
		 * and type vector of Double for output as a time series of frequencies..
		 */
	override def |> : PartialFunction[U, Try[V]] = {
		case xv: U if(xv.size >= 2 ) => fwrd(xv).map( _._2.toVector)
	}
	
	
   

		/**
		 * Extraction of frequencies from a time series using the Discrete Sine and Cosine 
		 * transforms.
		 * @param xv times series input to the Discrete Fourier Transform
		 * @return a tuple of Transformer instance and vector of frequencies.
		 * @note Exceptions thrown by the Apache Commons Math library
		 */
	protected def fwrd(xv: U): Try[(RealTransformer, DblArray)] = {
	  import Config._
	  
			// Select which Fourier series should be selected (even as Sine)
			// If the first value is 0.0 (or close) cosine series otherwise
		val rdt =  if( Math.abs(xv.head) < config)
			new FastSineTransformer(SINE)
		else 
			new FastCosineTransformer(COSINE)

			// Apply the forward transform to the padded time series
	  val padded = pad(xv.map(_.toDouble), xv.head == 0.0).toArray
	  Try( (rdt, rdt.transform(padded, FORWARD)) )
	}
}


		/**
		 * Companion object for the Discrete Fourier Cosine and Sine transform.
		 * @author Patrick Nicolas
		 * @since 0.98.1  February 12, 2014
		 * @version 0.98.2
		 * @see Scala for Machine Learning Chapter 3 "Data pre-processing"  Discrete Fourier transform
		 */
object DFT {

		/**
		 * Default constructor for the Discrete Fourier Transform
		 */
	def apply[T <: AnyVal](eps: Double)(implicit f: T => Double): DFT[T] = new DFT[T](eps)
	
	final val DFT_EPS = 1e-8
	def apply[T <: AnyVal](implicit f: T => Double): DFT[T] = new DFT[T](DFT_EPS)
}

	
// -----------------------  EOF --------------------------------------
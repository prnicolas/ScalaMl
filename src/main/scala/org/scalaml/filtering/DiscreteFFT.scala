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
package org.scalaml.filtering

	// Scala standard library
import scala.annotation.implicitNotFound
import scala.util.{Try, Success, Failure}
	// 3rd party libaries
import org.apache.commons.math3.transform._
import org.apache.log4j.Logger
	// ScalaMl classes
import org.scalaml.util.DisplayUtils
import org.scalaml.core.XTSeries
import org.scalaml.core.Design.PipeOperator
import org.scalaml.core.Types.ScalaMl
import DFT._, ScalaMl._

		/**
		 * <p>Generic Data transformation trait for the Fast Discrete Fourier. The main
		 * function of the trait is to pad time series to resize the original data set
		 * to the next power to 2. The internal method assumes that the implicit conversion
		 * from T to Double is defined.</p>
		 * @constructor Create a generic Fourier transform as a data transformation
		 * @see org.scala.commons.math3.transform
		 * @author Patrick Nicolas
		 * @since February 9, 2014
		 * @note Scala for Machine Learning Chapter 3 Data pre-processing / Discrete Fourier transform
		 */
trait DTransform[T] extends PipeOperator[XTSeries[T], XTSeries[Double]] {

	private[this] def padSize(xtSz: Int, even: Boolean= true): Int = {
		require(xtSz > 0, s"DTransform.padSize Cannot pad a series of size $xtSz")

			// Compute the size-boundaring for the padding
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
			  // if the 
			(if(even) (1<<bitPos) else (1<<bitPos)+1) - xtSz
		}
	}

		/**
		 * <p>Pads the input time series with zero values to reach the next
		 * boundary of 2 at power of N.<br> The input parameters are validated by 
		 * the client code.</p>
		 * @param xt input time series to filter
		 * @param even flag that specifies if the boundary for the data is an even number
		 * @param f implicit conversion of type T to Double
		 * @throws ImplicitNotFoundException if the implicit conversion T => Double is undefined
		 * @return New input array padded for the DFT
		 */
	@implicitNotFound("Conversion to Double is needed to pad a DFT transform")
	protected def pad(xt: XTSeries[T], even: Boolean=true)(implicit f: T => Double): DblVector = {
		val newSize = padSize(xt.size, even)
		val arr: DblVector = xt  // to force an implicit conversion
		
			// Fill up the remaining array with 0.0 if the size of the 
			// padding exceeds the size of the time series.
		if( newSize > 0) arr ++ Array.fill(newSize)(0.0) else arr
	}
}


		/**
		 * <p>Companion object to the class DTransform that define the <b>sinc</b> and <b>sinc2</b> 
		 * functions.</p>
		 * @author Patrick Nicolas
		 * @since February 9, 2014
		 * @note Scala for Machine Learning Chapter 2 Data pre-processing / Discrete Fourier transform
		 */
object DTransform { 
		/**
		 * Definition of the <b>sinc</b> convolution function used in discrete Fourier transform based 
		 * low pass filters
		 * @param f frequency 
		 * @param fC frequency cutoff of this low pass filter
		 * @return 1 if frequency is below cutoff, 0 otherwise
		 */
	def sinc(f: Double, fC: Double): Double = if(Math.abs(f) < fC) 1.0 else 0.0
	
		/**
		 * Definition of the sinc2 convolution function used in discrete Fourier transform based 
		 * low pass filters
		 * @param f frequency 
		 * @param fC frequency cutoff of this low pass filter
		 * @return 1 if frequency is below cutoff, 0 otherwise
		 */
	def sinc2(f: Double, fC: Double): Double = if(f*f < fC) 1.0 else 0.0
}

		/**
		 * <p>Discrete Fourier Transform for time series of element of type T bounded to a Double.
		 * This class inherit the generic DTransform to access the padding functions.</p>
		 * @constructor Create a Discrete Fourier transform
		 * @author Patrick Nicolas
		 * @since February 12, 2014
		 * @note Scala for Machine Learning Chapter 2 Data pre-processing / Discrete Fourier transform
		 */
protected class DFT[T <% Double] extends DTransform[T] {
	private val logger = Logger.getLogger("DFT")
	
		/**
		 * <p>Overload the pipe operator to compute the Discrete Fourier Cosine or Sine transform 
		 * (time series of frequency values). The type of transform is define at run-time the first 
		 * value is close to zero (Sine transform) or not (Cosine transform).</p>
		 * @throws MatchError if the input time series is undefined or its size is less or equal to 2
		 * @return PartialFunction of type vector of Double for input to the Discrete Fourier Transform, 
		 * and type vector of Double for output as a time series of frequencies..
		 */
	override def |> : PartialFunction[XTSeries[T], XTSeries[Double]] = {
		case xt: XTSeries[T] if( !xt.isEmpty ) => XTSeries[Double](fwrd(xt)._2)
	}
   

		/**
		 * <p>Extraction of frequencies from a time series using the Discrete Sine and Cosine 
		 * transforms.<br>	 
		 * Exception thrown by the Apache Commons Math library are caught at a higher level.</p>
		 * @param xt times series input to the Discrete Fourier Transform
		 * @return a tuple of Transformer instance and vector of frequencies.
		 */
	protected def fwrd(xt: XTSeries[T]): (RealTransformer, DblVector) = {
			// Select which Fourier series should be selected (even as Sine)
			// If the first value is 0.0 (or close) cosine series otherwise
		val rdt =  if( Math.abs(xt.head) < DFT_EPS)
			new FastSineTransformer(DstNormalization.STANDARD_DST_I)
		else 
			new FastCosineTransformer(DctNormalization.STANDARD_DCT_I)

			// Apply the forward transform to the padded time series
		(rdt, rdt.transform(pad(xt, xt.head == 0.0), TransformType.FORWARD))
	}
}


		/**
		 * Companion object for the Discrete Fourier Cosine and Sine transform.
		 * @author Patrick Nicolas
		 * @since February 12, 2014
		 * @note Scala for Machine Learning Chapter 2 Data pre-processing / Discrete Fourier transform
		 */
object DFT {
	final val DFT_EPS = 1e-20
		/**
		 * Default constructor for the Discrete Fourier Transform
		 */
	def apply[T <% Double]: DFT[T] = new DFT[T]
}

		/**
		 * <p>Low-band filter based of the Discrete Fourier transform. The overloaded Pipe Operator 
		 * implements the convolution of the filter function and the input time series class.<br>
		 * The class uses the Apache Commons Math library.</p>
		 * @param g   Filtering function y = g(x, fC)used in the convolution
		 * @param fC  Frequency cutoff for this low pass filter.
		 * @constructor Create a low-pass filter using the discrete Fourier transform
		 * @throws IllegalArgumentException if the cut-off value is out of bounds
		 * @author Patrick Nicolas
		 * @since February 9, 2014
		 * @note Scala for Machine Learning  Chapter 2 Data pre-processing / Fourier analysis / 
		 * DFT-based filtering
		 */
final class DFTFir[T <% Double](g: (Double, Double)=>Double, fC: Double) extends DFT[T] {
	require(fC > 0.0 && fC < 1.0, s"DFTFir Relative cutoff value $fC is incorrect")
   
	private val logger = Logger.getLogger("DFTFir")
   
		/**
		 * <p>Overload the pipe operator to compute the Discrete Fourier Cosine or Sine transform 
		 * (time series of frequency values). The type of transform is define at run-time the first 
		 * value is close to zero (Sine transform) or not (Cosine transform).</p>
		 * @throws MatchError if the input time series is undefined
		 * @return PartialFunction of time series of elements of type T as input to the Discrete 
		 * Fourier filter and time series of frequencies of type Double as output
		 * @param xt Parameterized time series for which the discrete transform has to be computed
		 */
	override def |> : PartialFunction[XTSeries[T], XTSeries[Double]] = {
		case xt: XTSeries[T] if( !xt.isEmpty ) => {
				// Forward computation of the discrete Fourier series
			val spectrum = fwrd(xt)
				// Computes the frequencies cut-off in data points
			val cutOff = fC*spectrum._2.size
				// Applies the cutoff to the original frequencies spectrum
			val filtered = spectrum._2.zipWithIndex.map(x => x._1*g(x._2, cutOff))
			
				// Reconstruct the signal.
			XTSeries[Double](spectrum._1.transform(filtered, TransformType.INVERSE) )
		}
	} 
}


// -----------------------  EOF --------------------------------------
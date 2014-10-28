/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.94
 */
package org.scalaml.filtering

import org.apache.commons.math3.transform._
import org.scalaml.core.{XTSeries, types}
import org.scalaml.core.design.PipeOperator
import org.apache.commons.math3.exception.MathIllegalArgumentException
import scala.Array.canBuildFrom
import scala.annotation.implicitNotFound
import types.ScalaMl._
import scala.util.{Try, Success, Failure}
import DFT._
import org.apache.log4j.Logger
import org.scalaml.util.Display


			/**
			 * <p>Generic Data transformation trait for the Fast Discrete Fourier. The main
			 * function of the trait is to pad time series to resize the original data set
			 * to the next power to 2. The internal method assumes that the implicit conversion
			 * from T to Double is defined.</p>
			 * @constructor Create a generic Fourier transform as a data transformation
			 * @author Patrick Nicolas
			 * @since February 9, 2014
			 * @note Scala for Machine Learning
			 */
trait DTransform[T] extends PipeOperator[XTSeries[T], XTSeries[Double]] {

   private[this] def padSize(xtSz: Int, even: Boolean= true): Int = {
  	 require(xtSz > 0, "Cannot pad a series of size " + xtSz + "  for the Discrete Fourier")
  	 
     val sz = if( even ) xtSz else xtSz-1
     if( (sz & (sz-1)) == 0)  
         0
     else {
        var bitPos = 0
        do {
          bitPos += 1
        }while( (sz >> bitPos) > 0)
        (if(even) (1<<bitPos) else (1<<bitPos)+1) - xtSz
     }
   }
   
   @implicitNotFound("Conversion to Double is needed to pad a DFT transform")
   protected def pad(xt: XTSeries[T], even: Boolean=true)(implicit f: T => Double): DblVector = {
  	  val newSize = padSize(xt.size, even)
  	  val arr: DblVector = xt  // to force an implicit conversion
  	  if( newSize > 0) arr ++ Array.fill(newSize)(0.0)  else arr
   }
}


		/**
		 * <p>Companion object to the class DTransform that define the sinc and sinc2 function.
		 */
object DTransform { 
  def sinc(f: Double, fC: Double): Double = if(Math.abs(f) < fC) 1.0 else 0.0
  def sinc2(f: Double, fC: Double): Double = if(f*f < fC) 1.0 else 0.0
}



	/**
	 * <p>Discrete Fourier Transform for time series of element of type T bounded to a Double.
	 * This class inherit the generic DTransform to access the padding functions.</p>
	 */
// class DFT[@specialized(Double) T <% Double] extends DTransform[T] {
class DFT[T <% Double] extends DTransform[T] {
	private val logger = Logger.getLogger("DFT")
	
		/**
		 * <p>Overload the pipe operator to compute the Discrete Fourier Cosine or Sine transform (time series
		 * of frequency values). The type of transform is define at run-time the first value is close to zero (Sine transform)
		 * or not (Cosine transform).</p>
		 * @param xt Parameterized time series for which the discrete transform has to be computed
		 * @throws MatchError if the input time series is undefined or its size is less or equal to 2
   		 * @return PartialFunction of type vector of Double for input to the Discrete Fourier Transform, and type vector of Double for output as a time series of frequencies..
		 */
   override def |> : PartialFunction[XTSeries[T], XTSeries[Double]] = {
  	 case xt: XTSeries[T] if(xt != null && xt.size > 2) => XTSeries[Double](fwrd(xt)._2)
   }
   

   		/**
   		 * <p>Extraction of frequencies from a time series using the Discrete Sine and Cosine transform.<br>	 
   		 * Exception thrown by the Apache Commons Math library are caught at a higher level.</p>
   		 * @param xt times series input to the Discrete Fourier Transform
   		 * @return a tuple of Transformer instance and vector of frequencies.
   		 */
   protected def fwrd(xt: XTSeries[T]): (RealTransformer, DblVector) = {
     val rdt =  if( Math.abs(xt.head) < DFT_EPS)
        new FastSineTransformer(DstNormalization.STANDARD_DST_I)
     else 
        new FastCosineTransformer(DctNormalization.STANDARD_DCT_I)
   
     (rdt, rdt.transform(pad(xt, xt.head == 0.0), TransformType.FORWARD))
   }
}


	
		/**
		 * Companion object for the Discrete Fourier Cosine and Sine transform.
		 */
object DFT {
   final val DFT_EPS = 1e-20
   def apply[T <% Double]: DFT[T] = new DFT[T]
}


			 /**
			 * <p>Pass-band Filter based of the Discrete Fourier transform. The overloaded Pipe Operator 
			 * implements the convolution of the filter function and the input time series class.<br>
			 * The class uses the Apache Commons Math library</p>
			 * @constructor Create a low-pass, band-pass or high-pass filter using the discrete Fourier transform: [g] filtering function used in the convolution
			 * @throws IllegalArgumentException if the filtering function g is undefined.
			 * @author Patrick Nicolas
			 * @since February 9, 2014
			 * @note Scala for Machine Learning
			 */
class DFTFir[T <% Double](g: (Double, Double)=>Double, fC: Double) extends DFT[T] {
   require(g != null, "Cannot apply a band pass filter with undefined filter function")
   require(fC > 0.0, s"Relative cutoff value $fC is incorrect")
   
   private val logger = Logger.getLogger("DFTFir")
   
	   /**
		 * <p>Overload the pipe operator to compute the Discrete Fourier Cosine or Sine transform (time series
		 * of frequency values). The type of transform is define at run-time the first value is close to zero (Sine transform)
		 * or not (Cosine transform).</p>
		 * @throws MatchError if the input time series is undefined
   		 * @return PartialFunction of time series of elements of type T as input to the Discrete Fourier filter and time series of frequencies of type Double as output
		 * @param xt Parameterized time series for which the discrete transform has to be computed
		 */
   override def |> : PartialFunction[XTSeries[T], XTSeries[Double]] = {
  	 case xt: XTSeries[T] if( xt != null && xt.size > 2) => {
	     val spectrum = fwrd(xt)
		 val filtered = spectrum._2.map( x => x*g(x, fC))
		 XTSeries[Double](spectrum._1.transform(filtered, TransformType.INVERSE) )
     }
   } 
}




// -----------------------  EOF --------------------------------------
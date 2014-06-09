/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.filtering

import org.apache.commons.math3.transform._
import org.scalaml.core.{XTSeries, Types}
import org.scalaml.workflow.PipeOperator
import org.apache.commons.math3.exception.MathIllegalArgumentException
import scala.Array.canBuildFrom
import scala.annotation.implicitNotFound
import Types.ScalaMl._


			/**
			 * <p>Generic Data transformation trait for the Fast Discrete Fourier. The main
			 * function of the trait is to pad time series to resize the original data set
			 * to the next power to 2.</p>
			 * 
			 * @author Patrick Nicolas
			 * @date February 9, 2014
			 * @project Scala for Machine Learning
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
  @implicitNotFound("fC threshold has to be defined for sinc")
  def sinc(f: Double)(implicit fC:Double): Double = if(Math.abs(f) < fC) 1.0 else 0.0
  
  @implicitNotFound("fC threshold has to be defined for sinc2")
  def sinc2(f: Double)(implicit fC:Double): Double = if(f*f < fC) 1.0 else 0.0
}




		/**
		 * <p>Fast Discrete Fourier Sine and Cosine transformation. The class is a data transformation
		 * (implements PipeOperator) and a Fourier Transform (implements DTransform). The class has a 
		 * specialized version for the type Double for faster computation.<br>
		 * The class uses the Apache Commons Math library</p>
		 * 
		 * @author Patrick Nicolas
		 * @date February 9, 2014
		 * @project Scala for Machine Learning
		 */
import DFT._
class DFT[@specialized(Double) T <% Double] extends DTransform[T] {
	
		/**
		 * <p>Overload the pipe operator to compute the Discrete Fourier Cosine or Sine transform (time series
		 * of frequency values). The type of transform is define at run-time the first value is close to zero (Sine transform)
		 * or not (Cosine transform).</p>
		 * @param xt Parameterized time series for which the discrete transform has to be computed
		 * @return The times series of the frequencies
		 * @exception IllegalArgumentException if the time series is not defined
		 * @exception MathIllegalArgumentException if the transform fails.
		 */
   override def |> (xt: XTSeries[T]): Option[XTSeries[Double]] = {
  	 try {
  		 Some(XTSeries[Double](compute(xt)._2))
  	 }
  	 catch {
  		 case e: MathIllegalArgumentException => println(e.toString); None
  	 }
   }

   		/**
   		 * Extraction of frequencies from a time series using the Discrete
   		 * Sine and Cosine transform
   		 */
   protected def compute(xt: XTSeries[T]) : (RealTransformer, DblVector) = {
  	 require(xt != null, "Cannot execute the Discrete Fourier transform on undefined time series")
  	 
  	 val rdt =  if( Math.abs(xt.head) < DFT_EPS)
        new FastSineTransformer(DstNormalization.STANDARD_DST_I)
     else 
        new FastCosineTransformer(DctNormalization.STANDARD_DCT_I)
   
     (rdt, rdt.transform( pad(xt, xt.head == 0.0),TransformType.FORWARD))
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
			 * @param g filtering function used in the convolution
			 * @exception IllegalArgumentException if the filtering function g is undefined.
			 * @author Patrick Nicolas
			 * @date February 9, 2014
			 * @project Scala for Machine Learning
			 */
class DFTFir[T <% Double](val g: Double=>Double) extends DFT[T] {
   require(g != null, "Cannot apply a band pass filter with undefined filter function")
   
	   /**
		 * <p>Overload the pipe operator to compute the Discrete Fourier Cosine or Sine transform (time series
		 * of frequency values). The type of transform is define at run-time the first value is close to zero (Sine transform)
		 * or not (Cosine transform).</p>
		 * @param xt Parameterized time series for which the discrete transform has to be computed
		 * @return The times series of the frequencies if transform succeeds, None otherwise
		 * @exception IllegalArgumentException if the time series is not defined
		 * @exception MathIllegalArgumentException if the transform fails.
		 */
   override def |> (xt: XTSeries[T]) : Option[XTSeries[Double]] = {
  	 try {
	  	  val rdtFreq = compute(xt)
	      val filtered = Array.tabulate(rdtFreq._2.size)(x =>g(x)).zip(rdtFreq._2).map( x => x._1 * x._2)
	      Some(XTSeries[Double](rdtFreq._1.transform(filtered, TransformType.INVERSE) ))
  	 }
  	 catch {
  		 case e: MathIllegalArgumentException => println(e.toString); None
  		 case e: RuntimeException => println(e.toString); None
  	 }
   }
}




// -----------------------  EOF --------------------------------------
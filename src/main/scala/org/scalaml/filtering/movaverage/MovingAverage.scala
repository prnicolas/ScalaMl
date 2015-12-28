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
package org.scalaml.filtering.movaverage

	// Scala standard library
import scala.annotation.implicitNotFound
import scala.util.Try

import org.scalaml.stats.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.ETransform
import org.scalaml.core.Design.{Config, ConfigInt}
import ScalaMl._, XTSeries._, SimpleMovingAverage._, WeightedMovingAverage._


		/**
		 * Parameterized moving average (view bound with Double) data transformation
		 * @tparam T type of element of input values (bounded to a Double)
		 * @constructor Generic moving average
		 * @author Patrick Nicolas
		 * @since February 7, 2014 0.98
		 * @version 0.98
		 * @see Scala for Machine Learning Chapter 3 Data Pre-processing / Moving averages
		 */
trait MovingAverage[T]


		/**
		 * Parameterized simple moving average data transformation. This data transformation is defined
		 * with an explicit configuration (weights) and implements ETransform. The numeric type 
		 * has to be implicitly defined in order to execute arithmetic operation on elements of the 
		 * time series.
		 * {{{
		 *    x'(t) = x'(t-1) + [x(t)-x(t-p)]/p   with  x' estimate of x
		 * }}}
		 * @tparam T type of element of input values (bounded to a Double)
		 * @constructor Create a simple moving average with a period '''period'''
		 * @param period period or size of the time window, p in the moving average
		 * @param num instance of Numeric type using for summation
		 * @throws IllegalArgumentExceptionif period is non positive
		 * @throws ImplicitNotFoundException if the numeric instance is not defined prior 
		 * instantiation of the moving average
		 * 
		 * @author Patrick Nicolas
		 * @since February 7, 2014 - 0.98
		 * @version 0.98.3  
		 * @see Scala for Machine Learning Chapter 3 "Data Pre-processing" / Moving averages
		 * @note 0.98.3: Simplify the implementation of |> and get using scanLeft higher order method.
		 */
@throws(classOf[IllegalArgumentException])
@implicitNotFound(msg = "SimpleMovingAverage Numeric bound has to be implicitly defined for $T")
protected class SimpleMovingAverage[T <: AnyVal](period: Int)
		(implicit num: Numeric[T], f: T => Double) 
	extends ETransform[Int](period) with MovingAverage[T] {
 
  require( period > 0 && period < 1e+4, 
			s"SimpleMovingAverage found period = $period required 0 < period < 1e+4]")

  
  type U = XSeries[T]
  type V = DblVector
  
  protected[this] val zeros = if(zerosPadded) Vector.fill(period-1)(0.0) else Vector.empty[Double]
	   
		/**
		 * Implementation of the data transformation of a time series of type T to a time series 
		 * of type Double using the simple moving average. This method invokes get method on array
		 * of Double values.
		 * @throws MatchError exception if the input time series is undefined
		 * @return Partial function with time series of type T as input and time series of type 
		 * Double as output.
		 */
	override def |> : PartialFunction[U, Try[V]] = {
		case xt: U if( !xt.isEmpty) => Try( get(xt))
	}
   

		/**
		 * Implementation of the data transformation of a vector of double values.
		 * @throws MatchError if the input time series is undefined
		 * @return PartialFunction of type vector of Double for input to the transformation, and 
		 * type vector of Double for output.
		 */
	def get : PartialFunction[XSeries[T], DblVector] = {
		case data: XSeries[T] if( data.size >= period ) => {
		  
				// Create a sliding window as a array of pairs of values (x(t), x(t-p))
			val splits = data.splitAt(period)
			val slider = data.take(data.size - period).zip( splits._2 )
			
				// 1. Compute the average value over the time window for the first 'period' data points
				// 2. Apply the sliding window 'slider' across the time series
			val zero = splits._1.sum/period
			
			val smoothed = slider.scanLeft(zero){ case (s, (x,y)) => s + (y - x)/period }
			if( !zeros.isEmpty ) zeros ++ smoothed else smoothed
		}
	}
}

		/**
		 * Companion object for the Simple moving average to define the constructor apply
		 * @author Patrick Nicolas
		 * @since 0.98  February 7, 2014
		 * @version 0.98.2
		 * @see Scala for Machine Learning Chapter 3 "Data Pre-processing" / Moving averages
		 */
object SimpleMovingAverage {
		/**
		 * Constructor for the SimpleMovingAverage class
		 * @tparam T type of element of input values (bounded to a Double)
		 * @param period Period or size of the time window, p in the moving average
		 * @param num implicit instance of Numeric type
		 * @return Simple moving average with a specific period
		 */
	def apply[T <: AnyVal](
			period: Int, 
			padded: Boolean = true)(implicit num: Numeric[T], f: T => Double): SimpleMovingAverage[T] = {

		zerosPadded = padded
		new SimpleMovingAverage[T](period)
	}
  
	def apply[T <: AnyVal](period: Int)(implicit num: Numeric[T], f: T =>Double): SimpleMovingAverage[T] = 
		new SimpleMovingAverage[T](period)
		
		
	 implicit var zerosPadded: Boolean = true
}

		/**
		 * Parameterized exponential moving average data transformation. This data transformation 
		 * is defined with an explicit configuration (weights) and implements ETransform
		 * 
		 * {{{
		 *  x'(t) = (1- alpha).x'(t-1) + alpha.x(t)  with x' is the estimate of x
		 * }}}
		 * @constructor Create an exponential moving average
		 * @tparam T type of element of input values (bounded to a Double)
		 * @param alpha Decay factor or coefficient of the exponential moving average.
		 * @throws IllegalArgumentException if period is non positive or alpha is out of range [0,1]
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98  February 7, 2014
		 * @version 0.98.2
		 * @see Scala for Machine Learning Chapter 3 "Data Pre-processing" / Moving averages
		 */
@throws(classOf[IllegalArgumentException])
@implicitNotFound(msg = "ExpMovingAverage Numeric bound has to be implicitly defined for $T")
final protected class ExpMovingAverage[@specialized(Double) T <: AnyVal](  
		alpha: Double)
		(implicit f: T => Double)
	extends ETransform[Double](alpha) with MovingAverage[T]  {
  
	require( alpha > 0.0 && alpha <= 1.0, 
			s"ExpMovingAverage found alpha = $alpha required > 0.0 and <= 1.0")
   

  type U = XSeries[T]
  type V = DblVector
  
  
		 /**
		  * Implementation of the data transformation, exponential moving average by overloading 
		  * the pipe operator.
		  * @throws MatchError if the input time series is undefined
		  * @return PartialFunction of time series of type T and a time series of Double elements 
		  * as output
		  */
	override def |> : PartialFunction[U, Try[V]] = {
		case xt: U if(xt.size > 1) => {
			val alpha_1 = 1-alpha
			var y: Double = xt(0)
			
				// Applies the exponential smoothing formula for each data point
			Try(xt.map(x => {
				val z = x*alpha + y*alpha_1
				y = z
				z 
			}))
		}
	}
}


	/**
	 * Companion object for the Exponential moving average to define the constructors apply
	 * @author Patrick Nicolas
	 * @since 0.98 February 7, 2014
	 * @version 0.99
	 * @see Scala for Machine Learning Chapter 3 "Data Pre-processing" / Moving averages
	 */
object ExpMovingAverage {
		/**
		 * Default constructor for the ExpMovingAverage class
		 * @tparam T type of element of input values (bounded to a Double)
		 * @param alpha Decay factor or coefficient pf the exponential moving average.
		 * @return Exponential moving average with a decay factor of '''alpha'''
		 */
  def apply[T <: AnyVal](alpha: Double)(implicit f: T => Double): ExpMovingAverage[T] = 
		new ExpMovingAverage[T](alpha)
		
		/**
		 * Constructor for the ExpMovingAverage class with alpha = 2/(1+p)
		 * @tparam T type of element of input values (bounded to a Double)
		 * @param period Period or size fo the time window in the moving average
		 * @return Exponential moving average with a decay factor of '''alpha=1/(2+period)'''
		 */
	def apply[T <: AnyVal](period: Int)(implicit f: T => Double): ExpMovingAverage[T] = 
		new ExpMovingAverage[T](2.0/(period+1))
}
		/**
		 * Parameterized weighted average data transformation. This data transformation is defined
		 * with an explicit configuration (weights) and implements ETransform
		 * {{{
		 *   x'(t) = { wt.x(t) + wt-1.x(t-1) + wt-2.x(t-2) + .... + wt-p.x(t-p) } /p
		 *   with p = weights.size
		 * }}}
		 * @constructor Create a weighted moving average with a predefined array of weights
		 * @param weights Weights (or coefficients) used in the time window
		 * @throws IllegalArgumentException if the weights are undefined or are not normalized
		 * 
		 * @author Patrick Nicolas
		 * @since  0.98 February 7, 2014
		 * @version 0.98.3
		 * @see Scala for Machine Learning Chapter 3 Data Pre-processing / Moving averages
		 * @note 0.98.3 Replace ''/:'' by ''map.sum''
		 */
@throws(classOf[IllegalArgumentException])
@implicitNotFound(msg = "WeightedMovingAverage Numeric bound has to be implicitly defined for $T")
final class WeightedMovingAverage[T <: AnyVal](weights: DblArray)
		(implicit num: Numeric[T], f: T => Double) 
	extends SimpleMovingAverage[T](weights.length) {

     
		/**
		 * Implementation of the data transformation.
		 * @tparam U type of input data
		 * @tparam V type of output data
		 * @throws MatchError if the number of weights exceeds the number of observations
		 * @return '''PartialFunction''' of type vector of '''Double''' for input to the transformation, and 
		 * type vector of '''Double''' for output.
		 */
  
	override def |> : PartialFunction[U, Try[V]] = {
		case xt: U if(xt.size >= weights.length ) => {
		  	
		  	// Compute the smoothed time series by apply zipping a
				// time window (array slice) and normalized weights distribution
				// and computing their dot product ...
			val smoothed =  (config to xt.size).map( i => 
				xt.slice(i- config, i).zip(weights).map { case (x, w) => x*w }.sum
			)
			
				// Create a time series with the weighted data points
			if( !zeros.isEmpty ) Try(zeros ++ smoothed) else Try(smoothed.toVector)
		}
	}
}


		/**
		 * Companion object for the Weighed moving average to define the constructor apply
		 * @author Patrick Nicolas
		 * @since 0.98  February 7, 2014
		 * @version 0.98.2
		 * @see Scala for Machine Learning Chapter 3 "Data Pre-processing" / Moving averages
		 */
object WeightedMovingAverage {
	final val PRECISION = 1e-4
		/**
		 * Default constructor for the weighted moving average
		 * @param weights Weights (or coefficients) used in the time window
		 * @return Weighted moving average with a predefined array of weights.
		 */
	def apply[T <: AnyVal](weights: DblArray)
			(implicit num: Numeric[T], f: T => Double): WeightedMovingAverage[T] = 
		new WeightedMovingAverage[T](weights)
}


// ----------------------------  EOF --------------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 */
package org.scalaml.filtering

import org.scalaml.core.XTSeries
import org.scalaml.core.types.ScalaMl._
import org.scalaml.core.design.PipeOperator
import scala.Array.canBuildFrom
import scala.annotation.implicitNotFound



		/**
		 * <p>Parameterized moving average (view bound with Double) data transformation</p>
		 * @constructor Generi moving average
		 * @author Patrick Nicolas
		 * @since February 7, 2014
		 * @note Scala for Machine Learning Chapter 3 Data Pre-processing / Moving averages
		 */
abstract class MovingAverage[T <% Double] extends PipeOperator[XTSeries[T], XTSeries[Double]] 


		/**
		 * <p>Parameterized simple moving average data transformation. The computation is implemented
		 * by the pipe operator |>. The numeric type has to be implicitly defined in order to execute
		 * arithmetic operation on elements of the time series.<br><br>
		 * <b>period</b> Period or duration of the window in the moving average<br>
		 * <b>num</b> Implicit numeric required for the summation of values of type T. </p>
		 * @constructor Create a simple moving average
		 * @throws IllegalArgumentExceptionif period is non positive
		 * @throws ImplicitNotFoundException if the numeric instance is not defined prior instantiation of the moving average
		 * 
		 * @author Patrick Nicolas
		 * @since February 7, 2014
		 * @note Scala for Machine Learning Chapter 3 Data Pre-processing / Moving averages
		 */
@implicitNotFound("SimpleMovingAverage Numeric bound has to be implicitly defined for the Simple moving average")
final protected class SimpleMovingAverage[@specialized(Double) T <% Double](period: Int)(implicit num: Numeric[T]) extends MovingAverage[T] {
	require( period > 0 && period < 1e+4, s"SimpleMovingAverage Cannot compute moving average with an incorrect $period")
   
		/**
		 * <p>Implementation of the data transformation of a time series of type T to a time series of type Double.</p>
		* @throws MatchError exception if the input time series is undefined
		 * @return Partial function with time series of type T as input and time series of type Double as output.
		 */
	override def |> : PartialFunction[XTSeries[T], XTSeries[Double]] = {
		case xt: XTSeries[T] if(xt != null && xt.size > 0) => {
			val slider = xt.take(xt.size - period).zip(xt.drop(period) )
			val a0 =  xt.take(period).toArray.sum/period
			var a: Double = a0

			// Computing each point by removing the first element in the time window
			// an adding the data point.
			val z = Array[Array[Double]]( 
				Array.fill(period)(0.0), a, slider.toArray.map(x => {a += (x._2 - x._1) /period; a })
			).flatten
			XTSeries[Double](z)   
		}
	}
   

		/**
		 * <p>Implementation of the data transformation of a vector of double values.</p>
		 * @throws MatchError if the input time series is undefined
		 * @return PartialFunction of type vector of Double for input to the transformation, and type vector of Double for output.
		 */
	def get : PartialFunction[DblVector, DblVector] = {
		case data: DblVector if(data != null && data.size > 0) => {
			val slider = data.take(data.size - period).zip(data.drop(period) )
			val a0 = data.take(period).sum/period
      
			var a = a0
			Array[Array[Double]]( 
				Array.fill(period)(0.0), a0,  slider.map(x => {a += (x._2 - x._1) /period; a }) 
			).flatten
		}
	}
}

		/**
		 * Companion object for the Simple moving average to define the constructor apply
		 * @author Patrick Nicolas
		 * @since February 7, 2014
		 * @note Scala for Machine Learning Chapter 3 Data Pre-processing / Moving averages
		 */
object SimpleMovingAverage {
	def apply[T <% Double](period: Int)(implicit num: Numeric[T]): SimpleMovingAverage[T] = new SimpleMovingAverage[T](period)
}

		/**
		 * <p>Parameterized exponential moving average data transformation. The computation is implemented
		 * by the pipe operator |>.<br><br>
		 * <b>period</b> Period of the window in the moving average.<br>
		 * <b>alpha</b> Decay factor for the exponential moving average.</p>
		 * @constructor Create an exponential moving average
		 * @throws IllegalArgumentException if period is non positive or alpha is out of range [0,1]
		 * @author Patrick Nicolas
		 * @since February 7, 2014
		 * @note Scala for Machine Learning Chapter 3 Data Pre-processing / Moving averages
		 */
final class ExpMovingAverage[@specialized(Double) T <% Double](period: Int, alpha: Double) extends MovingAverage[T]  {
	require( period > 0, s"ExpMovingAverage Cannot initialize exponential moving average with period = $period")
	require( alpha > 0 && alpha <= 1.0, s"ExpMovingAverage Cannot initialize exponential with alpha = $alpha")
   
		/**
		 * Constructor for the normalized exponential moving average for which alpha = 2.0/(period + 1)
		 * @param p period for the exponential moving average
		 */
	def this(p: Int)= this(p,2.0/(p+1))
  
		 /**
		  * <p>Implementation of the data transformation, exponential moving average by overloading the pipe operator.</p> 
		  * @throws MatchError if the input time series is undefined
		  * @return PartialFunction of time series of type T and a time series of Double elements as output
		  */
	override def |> : PartialFunction[XTSeries[T], XTSeries[Double]] = {
		case xt: XTSeries[T] if(xt != null && xt.size > 1) => {
			val alpha_1 = 1-alpha
			var y: Double = xt(0)
			
			xt.map(x => {val z = x*alpha + y*alpha_1; y = z;  z })
		}
	}
}


	/**
	 * Companion object for the Exponential moving average to define the constructors apply
	 * @author Patrick Nicolas
	 * @since February 7, 2014
	 * @note Scala for Machine Learning Chapter 3 Data Pre-processing / Moving averages
	 */
object ExpMovingAverage {
	def apply[T <% Double](period: Int, alpha: Double): ExpMovingAverage[T] = 
		new ExpMovingAverage[T](period, alpha)
		
	def apply[T <% Double](period: Int): ExpMovingAverage[T] = 
		new ExpMovingAverage[T](period)
}

		/**
		 * <p>Parameterized weighted average data transformation. The computation is implemented
		 * by the pipe operator |>.<br><br>
		 * <b>weights</b> Weights (or coefficients) used in the time window
		 * @constructor Create a weighted moving average
		 * @throws IllegalArgumentException if the weights are undefined or not normalized
		 * @author Patrick Nicolas
		 * @since February 7, 2014
		 * @note Scala for Machine Learning Chapter 3 Data Pre-processing / Moving averages
		 */
final class WeightedMovingAverage[@specialized(Double) T <% Double](weights: DblVector) extends MovingAverage[T]  {
	require( weights != null && Math.abs(weights.sum -1.0) < 1e-2, "WeightedMovingAverage Weights are not defined or normalized")
     
		/**
		 * <p>Implementation of the data transformation, weighted moving average by overloading the pipe operator.</p>
		 * @throws MatchError if the input time series is undefined
		 * @return PartialFunction of type vector of Double for input to the transformation, and type vector of Double for output.
		 */
	override def |> : PartialFunction[XTSeries[T], XTSeries[Double]] = {
		case xt: XTSeries[T] if(xt != null && xt.size > 1) => {
			val smoothed =  Range(weights.size, xt.size).map( i => {
				xt.toArray.slice(i- weights.size , i)
							.zip(weights)
							.foldLeft(0.0)((s, x) => s + x._1*x._2)
			})
			XTSeries[Double](Array.fill(weights.size)(0.0) ++ smoothed)
		}
	}
}


		/**
		 * Companion object for the Weighed moving average to define the constructor apply
		 * @author Patrick Nicolas
		 * @since February 7, 2014
		 * @note Scala for Machine Learning Chapter 3 Data Pre-processing / Moving averages
		 */
object WeightedMovingAverage {
	def apply[T <% Double](weights: DblVector): WeightedMovingAverage[T] = 
		new WeightedMovingAverage[T](weights)
}


// ----------------------------  EOF --------------------------------------------
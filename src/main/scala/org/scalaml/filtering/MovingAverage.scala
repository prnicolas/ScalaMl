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
	// ScalaMl classes
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.Design.PipeOperator
import ScalaMl._, XTSeries._

		/**
		 * <p>Parameterized moving average (view bound with Double) data transformation</p>
		 * @constructor Generic moving average
		 * @author Patrick Nicolas
		 * @since February 7, 2014
		 * @note Scala for Machine Learning Chapter 3 Data Pre-processing / Moving averages
		 */
abstract class MovingAverage[T <% Double] extends PipeOperator[XTSeries[T], DblSeries] 


		/**
		 * <p>Parameterized simple moving average data transformation. The computation is implemented
		 * by the pipe operator |>. The numeric type has to be implicitly defined in order to execute
		 * arithmetic operation on elements of the time series.</p>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;
		 * ,Arial,Helvetica,sans-serif;">
		 *  x'(t) = x'(t-1) + [x(t)-x(t-p)]/p  with x' estimate of x</span></pre>
		 * @constructor Create a simple moving average
		 * @param period Period or size of the time window, p in the moving average
		 * @param num instance of Numeric type using for summation
		 * @throws IllegalArgumentExceptionif period is non positive
		 * @throws ImplicitNotFoundException if the numeric instance is not defined prior 
		 * instantiation of the moving average
		 * 
		 * @author Patrick Nicolas
		 * @since February 7, 2014
		 * @note Scala for Machine Learning Chapter 3 Data Pre-processing / Moving averages
		 */
@implicitNotFound("SimpleMovingAverage Numeric bound has to be implicitly defined")
final protected class SimpleMovingAverage[@specialized(Double) T <% Double](
		period: Int)
		(implicit num: Numeric[T]) extends MovingAverage[T] {
  
	require( period > 0 && period < 1e+4, 
			s"SimpleMovingAverage Cannot compute moving average with an incorrect $period")
   
		/**
		 * <p>Implementation of the data transformation of a time series of type T to a time series 
		 * of type Double.</p>
		 * @throws MatchError exception if the input time series is undefined
		 * @return Partial function with time series of type T as input and time series of type 
		 * Double as output.
		 */
	override def |> : PartialFunction[XTSeries[T], DblSeries] = {
		case xt: XTSeries[T] if( !xt.isEmpty) => {
			
				// Create a sliding window as a array of pairs of 
				// values (x(t), x(t-p))
			val slider = xt.take(xt.size - period).zip(xt.drop(period) )
				// Compute the average value over the time window
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
		 * @return PartialFunction of type vector of Double for input to the transformation, and 
		 * type vector of Double for output.
		 */
	def get : PartialFunction[DblVector, DblVector] = {
		case data: DblVector if( !data.isEmpty ) => {
		  
				// Create a sliding window as a array of pairs of 
				// values (x(t), x(t-p))
			val slider = data.take(data.size - period).zip(data.drop(period) )
				// Compute the average value over the time window
			val a0 = data.take(period).sum/period

				// Apply the sliding window 'slider' across the time series
				// then flatten the matrix into a vector of DOuble
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
		/**
		 * Constructor for the SimpleMovingAverage class
		 * @param period Period or size of the time window, p in the moving average
		 * @param num implicit instance of Numeric type
		 */
	def apply[T <% Double](period: Int)(implicit num: Numeric[T]): SimpleMovingAverage[T] = 
		new SimpleMovingAverage[T](period)
}

		/**
		 * <p>Parameterized exponential moving average data transformation. The computation is 
		 * implemented by the pipe operator |>.
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;
		 * ,Arial,Helvetica,sans-serif;">
		 *  x'(t) = (1- alpha).x'(t-1) + alpha.x(t)  with x' is the estimate of x</span></pre></p>
		 * @constructor Create an exponential moving average
		 * @param period Period or size of the time window in the moving average
		 * @param alpha Decay factor or coefficient of the exponential moving average.
		 * @throws IllegalArgumentException if period is non positive or alpha is out of range [0,1]
		 * @author Patrick Nicolas
		 * @since February 7, 2014
		 * @note Scala for Machine Learning Chapter 3 Data Pre-processing / Moving averages
		 */
final protected class ExpMovingAverage[@specialized(Double) T <% Double](
		period: Int, 
		alpha: Double) extends MovingAverage[T]  {
  require( period > 0, 
			s"ExpMovingAverage Cannot initialize with incorrect value for period = $period")
	require( alpha > 0 && alpha <= 1.0, 
			s"ExpMovingAverage Cannot initialize with incorrect value for alpha $alpha")
   
		/**
		 * Constructor for the normalized exponential moving average for which alpha = 2.0/(period + 1)
		 * @param period period for the exponential moving average
		 */
	def this(period: Int)= this(period,2.0/(period+1))
  
		 /**
		  * <p>Implementation of the data transformation, exponential moving average by overloading 
		  * the pipe operator.</p> 
		  * @throws MatchError if the input time series is undefined
		  * @return PartialFunction of time series of type T and a time series of Double elements 
		  * as output
		  */
	override def |> : PartialFunction[XTSeries[T], DblSeries] = {
		case xt: XTSeries[T] if( !xt.isEmpty ) => {
			val alpha_1 = 1-alpha
			var y: Double = xt(0)
			
				// Applies the exponential smoothing formula for each data point
			xt.map(x => {
				val z = x*alpha + y*alpha_1
				y = z
				z 
			})
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
		/**
		 * Default constructor for the ExpMovingAverage class
		 * @param period Period or size fo the time window in the moving average
		 * @param alpha Decay factor or coefficient pf the exponential moving average.
		 */
	def apply[T <% Double](period: Int, alpha: Double): ExpMovingAverage[T] = 
		new ExpMovingAverage[T](period, alpha)

		/**
		 * Constructor for the ExpMovingAverage class with alpha = 1/(1+p)
		 * @param period Period or size fo the time window in the moving average
		 */
	def apply[T <% Double](period: Int): ExpMovingAverage[T] = 
		new ExpMovingAverage[T](period)
}

		/**
		 * <p>Parameterized weighted average data transformation. The computation is implemented
		 * by the pipe operator |>.
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;
		 * ,Arial,Helvetica,sans-serif;">
		 * x'(t) = { wt.x(t) + wt-1.x(t-1) + wt-2.x(t-2) + .... + wt-p.x(t-p) } /p</span></pre></p>
		 * @constructor Create a weighted moving average
		 * @param weights Weights (or coefficients) used in the time window
		 * @throws IllegalArgumentException if the weights are undefined or not normalized
		 * @author Patrick Nicolas
		 * @since February 7, 2014
		 * @note Scala for Machine Learning Chapter 3 Data Pre-processing / Moving averages
		 */
final class WeightedMovingAverage[@specialized(Double) T <% Double](
		weights: DblVector
		) extends MovingAverage[T]  {
  
	require( !weights.isEmpty && Math.abs(weights.sum -1.0) < 1e-2, 
			"WeightedMovingAverage Weights are not defined or normalized")
     
		/**
		 * <p>Implementation of the data transformation, weighted moving average by overloading 
		 * the pipe operator.</p>
		 * @throws MatchError if the input time series is undefined
		 * @return PartialFunction of type vector of Double for input to the transformation, and 
		 * type vector of Double for output.
		 */
	override def |> : PartialFunction[XTSeries[T], DblSeries] = {
		case xt: XTSeries[T] if( !xt.isEmpty) => {
			
				// Compute the smoothed time series by apply zipping a
				// time window (array slice) and normalized weights distribution
				// and computing their dot production
			val smoothed =  Range(weights.size, xt.size).map( i => {
				xt.toArray.slice(i- weights.size , i)
						.zip(weights)
						.foldLeft(0.0)((s, x) => s + x._1*x._2)  // dot product
			})
				// Create a time series with the weighted data points
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
		/**
		 * Default constructor for the weighted moving average
		 * @param weights Weights (or coefficients) used in the time window
		 */
	def apply[T <% Double](weights: DblVector): WeightedMovingAverage[T] = 
		new WeightedMovingAverage[T](weights)
}


// ----------------------------  EOF --------------------------------------------
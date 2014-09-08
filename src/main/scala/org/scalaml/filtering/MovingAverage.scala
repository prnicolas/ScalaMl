/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.filtering

import org.scalaml.core.{Types, XTSeries}
import org.scalaml.workflow.PipeOperator
import scala.Array.canBuildFrom
import scala.annotation.implicitNotFound


		/**
		 * <p>Parameterized moving average (view bound with Double) data transformation</p>
		 * @constructor Abstract moving average
		 * @author Patrick Nicolas
		 * @since February 7, 2014
		 * @note Scala for Machine Learning
		 */
abstract class MovingAverage[T <% Double] extends PipeOperator[XTSeries[T], XTSeries[Double]] 


		/**
		 * <p>Parameterized simple moving average data transformation. The computation is implemented
		 * by the pipe operator |>. The numeric type has to be implicitly defined in order to execute
		 * arithmetic operation on elements of the time series.</p>
		 * @constructor Create a simple moving average: [period] Period or duration of the window in the moving average
		 * @throws IllegalArgumentExceptionif period is non positive
		 * @author Patrick Nicolas
		 * @since February 7, 2014
		 * @note Scala for Machine Learning
		 */
@implicitNotFound("Numeric bound has to be implicitly defined for the Simple moving average")
class SimpleMovingAverage[@specialized(Double) T <% Double](val period: Int)(implicit num: Numeric[T]) extends MovingAverage[T] {
   import Types._, Types.ScalaMl._
   require( period > 0, "Cannot compute simple moving average with a null or negative period " + period)
   
   		/**
   		 * <p>Implementation of the data transformation by overloading the pipe operator.</p>
   		 * @param xt time series for which the simple moving average has to be computed.
   		 * @throws IllegalArgumentException if the input time series is undefined
   		 * @return Time series of element type double if computation succeeds, None otherwise
   		 */
   override def |> (xt: XTSeries[T]): Option[XTSeries[Double]] = {
      import Types.ScalaMl._
      require(xt != null && xt.size > 0, "Cannot compute simple moving average for undefined series")
      
      val slider = xt.take(xt.size - period).zip(xt.drop(period) )
      val a0 =  xt.take(period).toArray.sum/period
      var a: Double = a0
      
      	// Computing each point by removing the first element in the time window
        // an adding the data point.
      val z = Array[Array[Double]]( 
          Array.fill(period)(0.0), a,  slider.map(x => {a += (x._2 - x._1) /period; a }) 
      ).flatten
      
      Some(XTSeries[Double](z))
   }
   
   	     /**
   		 * <p>Implementation of the data transformation by overloading the pipe operator.</p>
   		 * @param data data sets for which the simple moving average has to be computed.
   		 * @throws IllegalArgumentException if the input time series is undefined
   		 * @return Time series of element type double if computation succeeds, None otherwise
   		 */
   def |> (data: DblVector): Option[DblVector] = {
      import Types.ScalaMl._
      val slider = data.take(data.size - period).zip(data.drop(period) )
      val a0 = data.take(period).sum/period
      
      var a = a0
      Some(Array[Array[Double]]( 
          Array.fill(period)(0.0), a0,  slider.map(x => {a += (x._2 - x._1) /period; a }) 
      ).flatten)
   }
}

	/**
	 * Companion object for the Simple moving average to define the constructor apply
	 * @author Patrick Nicolas
	 * @since February 7, 2014
	 * @note Scala for Machine Learning
	 */
object SimpleMovingAverage {
   def apply[T <% Double](period: Int)(implicit num: Numeric[T]): SimpleMovingAverage[T] = new SimpleMovingAverage[T](period)
}

		/**
		 * <p>Parameterized exponential average data transformation. The computation is implemented
		 * by the pipe operator |>.</p>
		 * @constructor Create an exponential moving average: [period] Period of the window in the moving average, [alpha] Decary factor
		 * @throws IllegalArgumentException if period is non positive or alpha is out of range [0,1]
		 * @author Patrick Nicolas
		 * @since February 7, 2014
		 * @note Scala for Machine Learning
		 */
final class ExpMovingAverage[@specialized(Double) T <% Double](private val period: Int, private val alpha: Double) extends MovingAverage[T]  {
  require( period > 0, "Cannot initialize exponential moving average with a null or negative period " + period)
  require( alpha > 0 && alpha <= 1.0, "Cannot initialize exponential with alpha " + alpha + " value")
   
  def this(p: Int)= this(p,2.0/(p+1))
  
		 /**
   		 * <p>Implementation of the data transformation, exponential moving average by overloading the pipe operator.</p>
   		 * @param xt time series for which the exponential moving average has to be computed.
   		 * @throws IllegalArgumentException if the input time series is undefined
   		 * @return Time series of element type double if computation succeeds, None otherwise
   		 */
  override def |> (data: XTSeries[T]): Option[XTSeries[Double]] = {
     val alpha_1 = 1-alpha
	 var y: Double = data(0)
	 Some(data.map( x => {val z = x*alpha + y*alpha_1; y = z;  z }))
  }
}


	/**
	 * Companion object for the Exponential moving average to define the constructors apply
	 * @author Patrick Nicolas
	 * @since February 7, 2014
	 * @note Scala for Machine Learning
	 */
object ExpMovingAverage {
   def apply[T <% Double](period: Int, alpha: Double): ExpMovingAverage[T] = new ExpMovingAverage[T](period, alpha)
   def apply[T <% Double](period: Int): ExpMovingAverage[T] = new ExpMovingAverage[T](period)
}


import Types.ScalaMl._
		/**
		 * <p>Parameterized weighted average data transformation. The computation is implemented
		 * by the pipe operator |>.</p>
		 * @constructor Create a weighted moving average: [weights] Weights (or coefficients) used in the time window
		 * @throws IllegalArgumentException if the weights are undefined or not normalized
		 * @author Patrick Nicolas
		 * @since February 7, 2014
		 * @note Scala for Machine Learning
		 */
final class WeightedMovingAverage[@specialized(Double) T <% Double](private val weights: DblVector) extends MovingAverage[T]  {
   require( weights != null && Math.abs(weights.sum -1.0) < 1e-2, "Weights are not defined or normalized")
     
   		/**
   		 * <p>Implementation of the data transformation, weighted moving average by overloading the pipe operator.</p>
   		 * @param xt time series for which the weighted moving average has to be computed.
   		 * @throws IllegalArgumentException if the input time series is undefined
   		 * @return Time series of element type double if computation succeeds, None otherwise
   		 */
    override def |> (xt: XTSeries[T]): Option[XTSeries[Double]] = {
      require(xt != null, "Cannot computed the weighted moving average for undefined time series")
      
      val smoothed =  Range(weights.size, xt.size).map( i => {
         xt.toArray.slice(i- weights.size , i).zip(weights).foldLeft(0.0)((s, x) => s + x._1*x._2)
      })

      Some(XTSeries[Double](Array.fill(weights.size)(0.0) ++ smoothed))
   }
}


	/**
	 * Companion object for the Weighed moving average to define the constructor apply
	 * @author Patrick Nicolas
	 * @since February 7, 2014
	 * @note Scala for Machine Learning
	 */
object WeightedMovingAverage {
  def apply[T <% Double](weights: DblVector): WeightedMovingAverage[T] = new WeightedMovingAverage[T](weights)
}


// ----------------------------  EOF --------------------------------------------
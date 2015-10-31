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
package org.scalaml.stats

import scala.Array.canBuildFrom
import scala.util.{Try, Success, Failure}
import scala.annotation.implicitNotFound

import org.apache.log4j.Logger
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.util.DisplayUtils
import Stats._


	/**
	 * Parameterized class that computes the generic minimun and maximum of a time series. The class
	 * implements:
	 * 
	 * - Computation of minimum and maximum according to scaling factors
	 * 
	 * - Normalization using the scaling factors
	 * @tparam T type of element of the time series view bounded to a double
	 * @constructor Create MinMax class for a time series of type ''XSeries[T]''
	 * @param values Time series of single element of type T
	 * @param f Implicit conversion from type T to Double
	 * @throws IllegalArgumentException if the time series is empty
	 * @throws implicitNotFoundException if the implicit convertion to Double is undefined
	 * 
	 * @author Patrick Nicolas
	 * @since 0.99  July 18, 2015
	 * @version 0.99
	 * @see Scala for Machine Learning Chapter 1 ''Getting Started''
	 */
@implicitNotFound(msg = "MinMax conversion to Double undefined")
@throws(classOf[IllegalArgumentException])
class MinMax[T <: AnyVal](val values: XSeries[T])(implicit f: T => Double) {
	require( !values.isEmpty, "MinMax: Cannot initialize stats with undefined values")
  
  
	def this(values: Array[T])(implicit f: T => Double) = this(values.toVector)

		/**
		 * Defines the scaling factors for the computation of minimum and maximum
		 * @param low lower value of the range target for the normalization
		 * @param high upper value of the range target for the normalization
		 * @param ratio  Scaling factor between the source range and target range.
		 */
	case class ScaleFactors(low: Double, high: Double, ratio: Double)
  
	private val logger = Logger.getLogger("MinMax")
  
	private[this] val zero = (Double.MaxValue, -Double.MaxValue)	
	private[this] var scaleFactors: Option[ScaleFactors] = None  
	
	protected[this] val minMax =  values./:(zero){(mM, x) => {
		val min = mM._1
		val max = mM._2
		(if(x < min) x else min, if(x > max) x else max)
  }}

		/**
		 * Computation of minimum values of a vector. This values is
		 * computed during instantiation
		 */
	final def min = minMax._1
	
		/**
		 * Computation of minimum values of a vector. This values is
		 * computed during instantiation
		 */
	final def max = minMax._2

	

	@throws(classOf[IllegalStateException])
	final def normalize(low: Double = 0.0, high: Double = 1.0): DblVector = 
		setScaleFactors(low, high).map(scale => {
	  	values.map( x =>(x - min)*scale.ratio + scale.low)
	})
	.getOrElse(throw new IllegalStateException("MinMax.normalize normalization params undefined"))
	
	
	
	final def normalize(value: Double): Try[Double] = Try {
		scaleFactors.map( scale => 
		  if(value <= min) scale.low
		  else if (value >= max) scale.high
		  else (value - min)*scale.ratio + scale.low
		).get
	}
	 
	 		/**
		 * Normalize the data within a range [l, h]
		 * @param l lower bound for the normalization
		 * @param h higher bound for the normalization
		 * @return vector of values normalized over the interval [0, 1]
		 * @throws IllegalArgumentException of h <= l
		 */
	private def setScaleFactors(low: Double, high: Double): Option[ScaleFactors] = 
		if( high < low + STATS_EPS)
			DisplayUtils.none(s"MinMax.set found high - low = $high - $low <= 0 required > ", logger)
		
		else {
			val ratio = (high - low)/(max - min)
		  
			if(ratio < STATS_EPS)
				DisplayUtils.none(s"MinMax.set found ratio $ratio required > EPS ", logger)
			else {
				scaleFactors = Some(ScaleFactors(low, high, ratio))
				scaleFactors
			}
		}
}



class MinMaxVector(series: Vector[DblArray]) {
	val minMaxVector: Vector[MinMax[Double]] = series.transpose.map(new MinMax[Double](_))
   
	@throws(classOf[IllegalStateException])
	final def normalize(low: Double = 0.0, high: Double = 1.0): Vector[DblArray] = 
		minMaxVector.map(_.normalize(low, high)).transpose.map(_.toArray)

    
	final def normalize(x: DblArray): Try[DblArray] = {
    val normalized = minMaxVector.zip(x).map{ case( from, to) => from.normalize(to) }
    
    if( normalized.contains( None) ) 
      throw new IllegalStateException("MinMax.normalize normalization params undefined")
    Try(normalized.map(_.get).toArray)
  }
}
	

object MinMax  {
	def apply[T <: AnyVal](values: XSeries[T])(implicit f: T => Double): Try[MinMax[T]] = 
		Try(new MinMax[T](values))
}



// -------------------------  EOF -----------------------------------------
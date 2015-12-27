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
package org.scalaml.stats

import scala.Array.canBuildFrom
import scala.util.Try
import org.scalaml.core.Types.ScalaMl._
import Stats._



		/**
		 *  Parameterized class that computes and update the statistics 
		 *  (mean, standard deviation) for any set of observations for which the
		 *  type can be converted to a Double.
		 *  
		 *  This class is immutable as no elements can be added to the original set of values.
		 *  @tparam T type of element of input data
		 *  @constructor Create an immutable statistics instance for a vector of type T 
		 *  @param values vector or array of elements of type T
		 *  @throws IllegalArgumentException if values is either undefined or have no elements
		 *  @author Patrick Nicolas
		 *  @since 0.98  Jan 24, 2014
		 *  @version 0.98.2
		 *  @see Scala for Machine Learning Chapter 2 "Hello World!" Profiling data
		 */
@throws(classOf[IllegalArgumentException])
class Stats[T <: AnyVal](values: XSeries[T])(implicit f: T => Double) extends MinMax[T](values){
	require( values.nonEmpty, "Stats: Cannot initialize stats with undefined values")
  
	private[this] val sums = values./:((0.0, 0.0))((acc, s) => (acc._1 + s, acc._2 + s*s))
	
		/**
		 * Arithmetic mean of the vector of values
		 */
	@inline
	lazy val mean = sums._1/values.size
	
		/**
		 * Computation of variance for the array values
		 */
	lazy val variance = (sums._2 - mean*mean*values.size)/(values.size-1)
		 
		/**
		 * Computation of standard deviation for the array values
		 */
	lazy val stdDev = Math.sqrt(variance)

	
		/**
		 * Compute the Lidstone smoothing factor for a set of values
		 * @param smoothing smoothing values ]0, 1] for Lidstone smoothing function
		 * @param dim Dimension of the model
		 * @return smoothed mean
		 * @throws IllegalArgumentException if either the smoothing or dimension of the model is 
		 * out of range
		 */
	final def lidstoneMean(smoothing: Double, dim: Int): Double = {
		require( smoothing >0.0 && smoothing <= 1.0, 
				s"Stats.lidstoneMean Lidstone smoothing factor $smoothing is out of range")
		require(dim > 0, s"Stats.lidstoneMean Dimension for Lidstone factor $dim is out of range")

		(sums._1 + smoothing)/(values.size + smoothing*dim)
	}
	
    
    	/**
		 * Compute the Laplace smoothing factor for a set of values
		 * @param dim smoothing correction factor ]0, 1] for Laplace smoothing function
		 * @return smoothed mean
		 * @throws IllegalArgumentException if the smoothing factor is out of range
		 */
	final def laplaceMean(dim: Int): Double = {
		require(dim > 0, s"Stats.laplaceMean Dimension for Lidstone factor $dim is out of range")
		(sums._1 + 1.0)/(values.size + dim)
	}

		
		/**
		 * Normalize the data set using the mean and standard deviation. It is assumed
		 * that the data (values) follows a Gaussian distribution
		 * @return vector of values transformed by the z-score
		 * @throws  ArithmeticException in case of a divide by zero
		 */
	@throws(classOf[IllegalStateException])
	def zScore: DblVector = {
	  if(stdDev <= STATS_EPS )
		   throw new IllegalStateException("Stats.normalize Cannot compute zScore -  divide by zero")
		values.map(x => (x - mean)/stdDev )
	}
	
	
		/**
		 * Compute the Gauss density function for a vector given a mean and standard deviation
		 * @return Gaussian probability 
		 * @throws IllegalArgumentExeption if stdDev is close t zero or the values are not defined.
		 */
	final def gauss(x: Double): Double = {
		val y = (x - mean)/stdDev
		INV_SQRT_2PI*Math.exp(-0.5*y*y)/stdDev
	}
}




		/**
		 * Companion object to the Statistics class that define the main constructor
		 * apply and the Gaussian distributions
		 * @author Patrick Nicolas
		 * @since January 24, 2014
		 * @note Scala for Machine Learning Chapter 2 Hello World!
		 */
object Stats {
	final val STATS_EPS = 1e-12
	final val INV_SQRT_2PI = 1.0/Math.sqrt(2.0*Math.PI)

		/**
		 * Default constructor for statistics
		 * @param values vector or array of elements of type T
		 */
	def apply[T <: AnyVal](values: Vector[T])(implicit f: T => Double): Stats[T] = 
		new Stats[T](values)
	
	
		/**
		 * Default constructor for statistics
		 * @param values vector or array of elements of type T
		 */
	def apply[T <: AnyVal](values: Array[T])(implicit f: T => Double): Stats[T] = 
		new Stats[T](values.toVector)
	
		/**
		 * Default constructor for statistics
		 * @param values vector or array of elements of type T
		 */
	def apply[T <: AnyVal](values: Iterator[T])(implicit f: T => Double): Stats[T] = 
		new Stats[T](values.toVector)

  
		/**
		 * Compute the Gauss density function for a value given a mean and standard deviation
		 * @param mean mean values of the Gauss probability density function
		 * @param stdDev standard deviation of the Gauss probability density function
		 * @param x  value for which the Gauss probability density function has to be computed
		 * @return Gaussian probability
		 * @throws IllegalArgumentException if stdDev is close t zero
		 */
	
	final def gauss(mean: Double, stdDev: Double, x: Double): Double = {
		require(Math.abs(stdDev) >= STATS_EPS, 
				s"Stats.gauss, Gauss standard deviation $stdDev is close to zero")
		
		val y = (x - mean)/stdDev
		INV_SQRT_2PI*Math.exp(-0.5*y*y)/stdDev
	}

	final val LOG_2PI = -Math.log(2.0*Math.PI)
	final def logGauss(mean: Double, stdDev: Double, x: Double): Double = {
		val y = (x - mean)/stdDev
		-LOG_2PI - Math.log(stdDev) - 0.5*y*y
	}
	
  val logNormal = logGauss(0.0, 1.0, _: Double)
   
		/**
		 * Compute the Gauss density value with a variable list of parameters
		 * @param x list of parameters
		 * @return Gaussian probability
		 * @throws IllegalArgumentExeption if stdDev is close to zero
		 */
	final def gauss(x: Double*): Double = {
		require(x.size > 2, s"Stats.gauss Number of parameters ${x.size} is out of range")
		gauss(x(0), x(1), x(2))
	}
  
  final def logGauss(x: Double*): Double = logGauss(x(0), x(1), x(2))
	

	
   
		/**
		 * Compute the Normal (Normalized Gaussian) density (mean = 0, standard deviation = 1.0)
		 * @return Gaussian probability
		 * @throws IllegalArgumentException if stdDev is close to zero or the number of parameters
		 * is less than 3
		 */
	val normal = gauss(0.0, 1.0, _: Double)

		/**
		 * Compute the Bernoulli density given a mean and number of trials
		 * @param mean mean value
		 * @param p Number of trials
		 */
	final def bernoulli(mean: Double, p: Int): Double = mean*p + (1-mean)*(1-p)

		/**
		 * Compute the Bernoulli density given a mean and number of trials with a variable list 
		 * of parameters
		 * @param x list of parameters
		 * @return Bernoulli probability 
		 * @throws IllegalArgumentException if the number of parameters is less than 3
		 */
	@throws(classOf[IllegalArgumentException])
	final def bernoulli(x: Double*): Double = {
		require(x.size > 2, s"Stats.bernoulli found ${x.size} arguments required > 2")
		bernoulli(x(0), x(1).toInt)
	}
}


// -------------------------  EOF -----------------------------------------
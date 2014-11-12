/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95d
 */
package org.scalaml.stats

import scala.Array.canBuildFrom
import org.scalaml.core.types.ScalaMl._
import Stats._

	/**
	 *  Parameterized class (view bound) that compute and update the statistics (mean,
	 *  standard deviation) for any set of observations for which the
	 *  type can be converted to a Double.
	 *  @constructor Create an immutable statistics instance for a vector of type T  [values] vector of type bounded to a double 
	 *  @param values vector or array of elements of type T
	 *  @throws IllegalArgumentException if values is either undefined or have no elements
	 *  @author Patrick Nicolas
	 *  @since Jan 24, 2014
	 *  @note Scala for Machine Learning
	 */
class Stats[T <% Double](values: DVector[T]) {
    require( values != null && values.size > 0, "Cannot initialize stats with undefined data")
	 
	private[this] var counters = values.foldLeft((Double.MaxValue, Double.MinValue, 0.0, 0.0))((c, x) => {
	  	            (if(x < c._1) x else c._1, if(x > c._2) x else c._2,  c._3 + x, c._4 + x*x ) })
    
	  	/**
	  	 * Arithmetic mean of the vector of values
	  	 */
	@inline
	lazy val mean = counters._3/values.size
	    /**
	     * Computation of variance for the array values
	     */
	lazy val variance = (counters._4 - mean*mean*values.size)/(values.size-1)
		 /**
	     * Computation of standard deviation for the array values
	     */
	lazy val stdDev = if(variance < ZERO_EPS) ZERO_EPS else Math.sqrt(variance)
		/**
	     * Computation of minimun values of a vector. This values is
	     * computed during instantiation
	     */
	lazy val min = counters._1
		/**
	     * Computation of minimun values of a vector. This values is
	     * computed during instantiation
	     */
	lazy val max = counters._2
	
		/**
		 * Compute the Lidsstone smoothing factor for a set of values
		 * @param smoothing smoothing values ]0, 1] for Lidstone smoothing function
		 * @param dim Dimension of the model
		 * @throws IllegalArgumentException if either the smoothing or dimension of the model is out of range
		 */
	def lidstoneMean(smoothing: Double, dim: Int): Double = {
    	require( smoothing >0.0 && smoothing <= 1.0, "Lidstone smoothing factor " + smoothing + " is out of range")
    	require(dim > 0, "Dimension for Lidstone factor " + dim + " is out of range")

    	(counters._3 + smoothing)/(values.size + smoothing*dim)
    }
	
    
    	/**
		 * Compute the Laplace smoothing factor for a set of values
		 * @param smoothing smoothing values ]0, 1] for Laplace smoothing function
		 * @throws IllegalArgumentException if the smoothing factor is out of range
		 */
    def laplaceMean(dim: Int): Double = (counters._3 + 1.0)/(values.size + dim)

		/**
		 * Fast normalization of values within a range of [0, 1]
		 * @throws throw a Aritmetic exception if the min and max have identical values
		 */
	def normalize: DblVector = {
	   val range = max - min
	   
	   if( range < ZERO_EPS) 
	  	  throw new ArithmeticException(s"Stats.normalize: cannot normalize $min and $max")
	   values.map(x => (x - min)/range)
	}
    
    	/**
    	 * Normalization of values within a range [-0.5. 0.5]
    	 */
    def normalizeMean: DblVector = normalize(-0.5, 0.5)
    
    	/**
    	 * Normalize the data within a range [l, h]
    	 * @param l lower bound for the normalization
    	 * @param h higher bound for the normalization
    	 * @throws IllegalArgumentException of h <= l
    	 */
    def normalize(l: Double, h: Double): DblVector = {
    	require(h > l + ZERO_EPS, s"Stats.normalize: cannot normalize between $l and $h")
    	val range = h-l
    	values.map( x =>(x - l)/range)
    }
    
	   /**
	    * Normalize the data set using the mean and standard deviation. It is assumed
	    * that the data (values) follows a Gaussian distribution
	    * @throws  ArithmeticException in case of a divide by zero
	    */
    def zScore: DblVector = {
       val factor = (max - min)*stdDev
	   
	   if( factor < ZERO_EPS) 
	  	  throw new ArithmeticException ("Cannot compute the standard score divide by zero")
       values.map(x => (x - min)/factor )
    }
}

		/**
		 * Companion object to the Statistics class that define the main constructor
		 * apply and the Gaussian distributions
		 * @author Patrick Nicolas
		 * @since January 24, 2014
		 * @note Scala for Machine Learning
		 */
object Stats {
   final val ZERO_EPS = 1e-32
   final val INV_SQRT_2PI = 1.0/Math.sqrt(2.0*Math.PI)
   
   def apply[T <% Double](values: Array[T]): Stats[T] = new Stats[T](values)

   		/**
   		 * <p>Compute the Gauss density function for an array of values.</p>
   		 * @param mean mean values of the Gauss pdf
   		 * @param stdDev standard deviation of the Gauss pdf'
   		 * @param values  array of variables for which the Gauss pdf has to be computed
   		 * @throws IllegalArgumentExeption if stdDev is close t zero or the values are not defined.
   		 */
   final def gauss(mean: Double, stdDev: Double, values: DblVector) : DblVector = {
      require(Math.abs(stdDev) >= ZERO_EPS, "Gauss standard deviation is close to zero")
      require(values != null, "Values for the Gauss distribution is undefined")
      
  	  values.map( x =>{val y = x - mean; INV_SQRT_2PI/stdDev * Math.exp(-0.5*y*y/stdDev)} )
   }
  
      	 /**
   		 * <p>Compute the Gauss density function for a floating point value</p>
   		 * @param mean mean values of the Gauss pdf
   		 * @param stdDev standard deviation of the Gauss pdf'
   		 * @param value  value for which the Gauss pdf has to be computed
   		 * @throws IllegalArgumentExeption if stdDev is close t zero
   		 */
   final def gauss(mean: Double, stdDev: Double, x:Double): Double = {
  	  require(Math.abs(stdDev) >= ZERO_EPS, s"Stats.gauss, Gauss standard deviation $stdDev is close to zero")
  	  val y = x - mean
  	  INV_SQRT_2PI/stdDev * Math.exp(-0.5*y*y /stdDev)
   }
   
   final def gauss(x: Double*): Double = gauss(x(0), x(1), x(2))
   
   final def normal(x: Double*): Double = gauss(0.0, 1.0, x(0))

   final def bernoulli(mean: Double, p: Int): Double = mean*p + (1-mean)*(1-p)
   
   @inline
   final def bernoulli(x: Double*): Double = bernoulli(x(0), x(1).toInt)
}

// -------------------------  EOF -----------------------------------------
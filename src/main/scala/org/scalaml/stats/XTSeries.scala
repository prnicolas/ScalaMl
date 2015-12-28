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

import scala.annotation.implicitNotFound
import scala.collection.immutable.VectorBuilder
import scala.util.Try
import scala.reflect.ClassTag
import scala.language.implicitConversions

import org.apache.log4j.Logger

import org.scalaml.core.Types.ScalaMl._




		/**
		 * Companion object for time series, that defined the essential functions on time series such
		 * as statistics, transpose, zScore, normalize.... This singleton implements basic type 
		 * conversion (implicit)
		 * @author Patrick Nicolas
		 * @since 0.98 January, 22, 2014 
		 * @version 0.99.1
		 * @see Scala for Machine Learning Chapter 3 "Data pre-processing" Time series
		 * @note Added ''splitAt'' method in 0.98.3, Added ''zipWithShift'', ''delta'', ''binaryDelta'',
		 * ''sse'' and ''mse'' in 0.99 
		 */
object XTSeries {
	final val EPS = 1-20  

	
	private val logger = Logger.getLogger("XTSeries")	
 	
		/**
		 * Generate a time series of tuple of type (T,T) by zipping the
		 * - last ''size - n'' observations of this time series with the 
		 * - first ''size - n'' observations
		 * @tparam T type of element (or data point or observation) of the time series
		 * @param xv Single variable vector to be zipped
		 * @param n Number of observations used in the shift
		 * @throws IllegalArgumentException if the shift parameter n is out of range
		 * @return time series of pairs from shifted Vectors.
		 */
	@throws(classOf[IllegalArgumentException])
	def zipWithShift[T](xv: XSeries[T], n: Int): Vector[(T, T)] = {
		require( n > 0 && n < xv.size,  
			s"XTSeries.zipWithShift found shift n= $n required n < ${xv.size}")  

		xv.drop(n).zip(xv.view.dropRight(n))
	}
	
		/**
		 * Generate a time series of tuple of type (T,T) by zipping the
		 * last ''size - n'' observations of this time series with the 
		 * first ''size - n'' observations
		 * @tparam T type of element (or data point or observation) of the time series
		 * @param xv Single variable array to be zipped
		 * @param n Number of observations used in the shift
		 * @throws IllegalArgumentException if the shift parameter n is out of range
		 * @return time series of pairs from shifted arrays
		 */
  @throws(classOf[IllegalArgumentException])
	def zipWithShift[T](xv: Array[T], n: Int): Array[(T, T)] = {
		require( n > 0 && n < xv.length,  
			s"XTSeries.zipWithShift found shift n= $n required n < ${xv.length}")

		xv.drop(n).zip(xv.view.dropRight(n))
	}
  
		/**
		 * Generate a time series of tuple of type (T,T) by zipping the time series
		 * with itself 
		 * @tparam T type of element (or data point or observation) of the time series
		 * @param xv Single variable vector to be zipped
		 * @return time series of pairs from shifted Vectors.
		 */
	def zipWithShift1[T](xv: XSeries[T]): Vector[(T, T)] = xv.zip(xv.view.drop(1))
	
		/**
		 * Generate a time series of tuple of type (T,T) by zipping the time series
		 * with itself 
		 * @tparam T type of element (or data point or observation) of the time series
		 * @param xv Single variable vector to be zipped
		 * @return time series of pairs from shifted Vectors.
		 */
	def zipWithShift1[T](xv: Array[T]): Array[(T, T)] = xv.zip(xv.view.drop(1))

  	/**
		 * Splits this time series into two distinct time series at a given index '''n'''
		 * @tparam T type of element (or data point or observation) of the time series
		 * @param xv Multi-variable vector to be splitted
		 * @param n index in the time series used in the split
		 * @return 2-tuple or pair of times series ''(ts1, ts2)'' ts1 containing n first elements
		 * in the original time series, ts2 containing the remaining elements
		 * @throws IllegalArgumentException if parameter n is out of bounds
		 */
	@throws(classOf[IllegalArgumentException])
	def splitAt[T](xv: XSeries[T], n: Int): (XSeries[T], XSeries[T]) = {
		require( n > 0 && n < xv.size, s"XTSeries.splitAt found index $n required index < ${xv.size}")

		val splitArr = xv.splitAt(n)
		(splitArr._1, splitArr._2)
	}
	
   
			/**
			 * Retrieve the dimension of the time series that is the number of variable in
			 * each observations or data points
			 * @tparam T type of element (or data point or observation) of the time series
			 * @param xt time series of arrays
			 * @return number of features in the observations of the time series
			 */
	def dimension[T](xt: XVSeries[T]): Int = xt.head.length
	
	
	
	implicit def xvseries2Vector[T <: AnyVal](xv: XVSeries[T])
			(implicit f: T => Double): Vector[DblArray] = 
		xv.map( _.map(_.toDouble))
	 

		/**
		 * Implements the normalization of a parameterized time series
		 * @tparam T type of element (or data point or observation) of the time series
		 * @param xt single dimension parameterized time series
		 * @throws IllegalArgumentException if the time series is undefined
		 * @throws implicitNotFound if the implicit ordering is undefined
		 * @return normalized time series as double elements if max > min, None otherwise
		 */
	@implicitNotFound(msg = "XTSeries.normalize convertion from $T to Double undefined")
	@throws(classOf[IllegalStateException])
	@throws(classOf[IllegalArgumentException])
	def normalize[T <: AnyVal](
			xt: XSeries[T], 
			low: Double, 
			high: Double)(implicit ordering: Ordering[T], f: T => Double): Try[DblVector] = 	
		Try (Stats[T](xt).normalize(low, high) )
	

	@implicitNotFound(msg = "XTSeries.normalize convertion from $T to Double undefined")
	@throws(classOf[IllegalStateException])
	@throws(classOf[IllegalArgumentException])
	def normalize[T <: AnyVal](xt: XSeries[T])
			(implicit ordering: Ordering[T], f: T => Double): Try[DblVector] = 
		normalize(xt, 0.0, 1.0)


   
		/**
		 * Implements the normalization of a parameterized multi-dimension time series within [0, 1]
		 * @tparam T type of element (or data point or observation) of the time series
		 * @param xt multi-dimension parameterized time series
		 * @throws IllegalArgumentException if the time series is undefined
		 * @return normalized time series as double elements if max > min, None otherwise
		 */
	@throws(classOf[IllegalArgumentException])
	@implicitNotFound(msg = "XTSeries.normalize conversion from $T to Double undefined")
	def normalize[T <: AnyVal](
			xt: XVSeries[T])
			(implicit order: Ordering[T], m: Manifest[T], f: T => Double): Try[Vector[DblArray]] = {
	  
	  require( xt.nonEmpty,
				"XTSeries.normalize Cannot normalize an undefined time series of elements")
		require( dimension(xt) > 0, 
				"XTSeries.normalize Incorrect function to normalize a single dimension time series")
    
		var k = 0
			val res = new Array[Array[T]](xt.size)
		val dim = dimension(xt)

		val min = Array.fill(dim)( Double.MaxValue)
		val max = Array.fill(dim)(-Double.MaxValue)
		
		val _xv = xt.toVector
     
			// computes min and max
		while( k < xt.size) {
			var j = 0
			while( j < dim) {
				if( _xv(k)(j) < min(j)) 
					min(j) = _xv(k)(j)
				else if( _xv(k)(j) > max(j)) 
					max(j) = _xv(k)(j)
				j += 1
			}
			k += 1
		}

    val data = new VectorBuilder[DblArray]    
		k = 0
     
		Try {
			while( k < xt.size) {
				var j = 0
				val arr = new Array[Double](dim)
				while( j < dim) {
					arr(j) =(_xv(k)(j) - min(j))/(max(j)-min(j))
					j += 1
				}
        data += arr
				k += 1
			}
			data.result()
		}
	}
  
  
	@throws(classOf[IllegalArgumentException])
	def zScore[T <: AnyVal](xt: XSeries[T])(implicit f: T => Double): Try[DblVector] = Try(Stats[T](xt).zScore )
	

		/**
		 * transform time series of parameterized array into a array of double Vector
		 * by applying the '''Z score''' transform to each element of the time series.
		 * @tparam T type of element (or data point or observation) of the time series
		 * @param xt multi-dimensional parameterized time series
		 * @throws IllegalArgumentException if the time series is undefined
		 * @return Time series of double array if the function succeeds, None otherwise
		 */
  @throws(classOf[IllegalArgumentException])
	def zScores[T <: AnyVal](xt: XVSeries[T])(implicit f: T => Double): Try[XVSeries[Double]] = {
		require( xt.nonEmpty, "XTSeries.zScoring Cannot zScore an undefined time series")
  	import scala.collection.immutable.VectorBuilder
    
		val stats = statistics(xt)
		var k = 0
		val dimension = xt.head.length

    val data = new VectorBuilder[DblArray]
    
		Try {
			while( k < xt.size) {
				var j = 0
				val arr = Array.fill(dimension)(0.0)
				while( j < dimension) {
					arr(j) =(xt(k)(j) - stats(j).mean)/stats(j).stdDev
					j += 1
				}
        data += arr
				k += 1
			}
			data.result()
		}
	}
	
	def unit[T <: AnyVal](xt: XVSeries[T])(implicit f: T => Double): Try[Vector[DblArray]] = 
		Try(xt.map(_.map( _.toDouble)))
	

	def zipToXVSeries[T](x: XVSeries[T], y: XVSeries[T])
		(f: (Array[T], Array[T]) => Double): XSeries[Double] = {
		require( x.size == y.size,  
			s"XTSeries.zipSeries found x.size = ${x.size} != y.size  ${y.size}")
		x.zip(y.view).map{ case (_x, _y) => f(_x, _y)}
	}
	
	def zipToXSeries[T](x: Vector[T], y: Vector[T])(f: (T, T) => Double): XSeries[Double] = {
		require( x.size == y.size,  
			s"XTSeries.zipSeries found x.size = ${x.size} != y.size  ${y.size}")
		x.zip(y.view).map{ case (_x, _y) => f(_x, _y)}
	}
	
	def zipToArray[T](x: Array[T], y: Array[T])(f: (T, T) => Double): DblArray = {
		require( x.length == y.length,
			s"XTSeries.zipSeries found x.length = ${x.length} != y.length  ${y.length}")
		x.zip(y.view).map{ case (_x, _y) => f(_x, _y)}
	}
	
	
	def zipToSeries[T](x: Vector[T], y: Vector[T])(implicit f: T => Double): XVSeries[Double] = {
		require( x.size == y.size,  
			s"XTSeries.zipSeries found x.size = ${x.size} != y.size  ${y.size}")
		x.zip(y.view).map{ case (_x, _y) => Array[Double](_x, _y)}
	}
	
	
	def zipToSeries[T: ClassTag](x: Vector[T], y: Vector[T], nSteps: Int)
			(implicit f: T => Double): XVSeries[Double] = {
		require( nSteps > 0, s"XTSeries.zipSeries found nSteps = $nSteps, required > 0" )
		require( x.size == y.size,  
			s"XTSeries.zipSeries found x.size = ${x.size} != y.size  ${y.size}")
			
		x.zip(y.view).map{ case (_x, _y) => Array[Double](_x, _y)}.dropRight(nSteps)
	}

	def inner[T <: AnyVal](xt: Array[T], zt: DblArray)(implicit f: T => Double): Double = 
		xt.zip(zt).map{ case (_x, z) => _x*z }.sum

	def transform[T: ClassTag](xt: XVSeries[T]): Try[XVSeries[T]] = Try(xt.transpose.map(_.toArray))

		/**
		 * Compute the basic aggregate statistics for a time series
		 * @tparam T type of element (or data point or observation) of the time series
		 * @param xt time series for which the statistics are computed
		 * @return Statistics instance
		 */
	def statistics[T <: AnyVal](xt: XSeries[T])(implicit f: T => Double): Stats[T] = Stats[T](xt)
	

			/**
		 * Compute the basic statistics for each dimension of a time series
		 * @tparam T type of element (or data point or observation) of the time series
		 * @param xt time series for which the statistics are computed
		 * @return Array of statistics for each dimension
		 */
  @throws(classOf[IllegalArgumentException])
	def statistics[T <: AnyVal](xt: XVSeries[T])(implicit f: T => Double): Vector[Stats[T]] = {
		require( xt.nonEmpty || dimension(xt) > 0, "XTSeries.statistics input time series undefined")
		xt.transpose.map( Stats[T]( _ ))
	}
}


// ---------------------------------  EOF --------------------------------------------------------
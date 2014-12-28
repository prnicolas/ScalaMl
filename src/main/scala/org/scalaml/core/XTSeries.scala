/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied..
 * 
 * Version 0.98
 */
package org.scalaml.core

import scala.annotation.implicitNotFound
import scala.collection._
import scala.util.{Try, Success, Failure}
import scala.reflect.ClassTag
import scala.language.implicitConversions

import org.apache.log4j.Logger

import org.scalaml.core.Types.ScalaMl.{DblVector, DblMatrix, DVector}
import org.scalaml.stats.Stats
import org.scalaml.util.DisplayUtils


		/**
		 * <p>Generic class for time series. Any type from different libraries are converted into 
		 * this generic type to avoid multiple conversion between numerous types.
		 * The class is parameterized so it can take primitive types to create vector for single
		 * variable time series or arrays/list to create matrix for multiple variables time series.</p>		 * 
		 * @constructor Create a new parameterized time series XTSeries[T] with a label(id) and 
		 * an array of values: 
		 * @throws IllegalArgumentException If the array of values, arr is undefined
		 * @param label Name for the time series (optional)
		 * @param arr Array of values of the parameterized T
		 * 
		 * @author Patrick Nicolas
		 * @since January, 22, 2014
		 * @note Scala for Machine Learning Chapter 3 Data pre-processing / Time series
		 */
class XTSeries[T](val label: String, arr: Array[T]) { 
	import XTSeries._
	require( !arr.isEmpty, "XTSeries Cannot create a times series with undefined values")
  
	final def toArray: Array[T] = arr
	final def toList: List[T] = arr.toList
  
	final def head: T = arr.head
	final def last: T = arr.last
  
		/**
		 * Test if a time series is identical to this time series. The label is not included
		 * in the comparison.
		 * @param that other series  this series is compared to
		 * @throws IllegalArgumenException if the other time series is undefined
		 * @return true if the series are identical, false if the other time series, 'that' is null 
		 * or is different from this time series
		 */
	def == (that: XTSeries[T]): Boolean = {
		require( !that.isEmpty, 
				"XTSeries.== Cannot compare this time series with undefined time series")
		size == that.size && arr.equals(that.toArray)
	}
  
        
		/**
		 * <p>Convert a this time series into a vector of Double floating point values.</p>
		 * @param f  implicit conversion of type T to Double
		 * @return Vector of double values (DblVector)
		 * @throws implicitNotFound if the implicit conversion is undefined
		 */
	@implicitNotFound("Conversion from type T to DblVector undefined")
	def toDblVector(implicit f: T => Double): DblVector = arr.map( f( _ ) )
  
		/**
		 * <p>Convert a this time series into a matrix of Double floating point values.</p>
		 * @param fv  implicit conversion of type T to DblVector
		 * @return Matrix of double values (DblMatrix)
		 * @throws implicitNotFound if the implicit conversion is undefined
		 */
	@implicitNotFound("Conversion from type T to DblMatrix undefined")
	def toDblMatrix(implicit fv: T => DblVector): DblMatrix = arr.map( fv( _ ) )
  
	def + (n: Int, t: T)(implicit f: (T,T) => T): T = f(arr(n), t)
  
	@inline
	def tail: XTSeries[T] = new XTSeries(label, arr.tail)
  
	def take(n: Int): XTSeries[T] = new XTSeries(label, arr.take(n))
	def takeRight(n: Int): XTSeries[T] = new XTSeries(label, arr.takeRight(n))
  
	def drop(n: Int): XTSeries[T] = new XTSeries(label, arr.drop(n))
	
	def dropRight(n: Int): XTSeries[T] = new XTSeries(label, arr.dropRight(n))
  
	def map[U: ClassTag](f: T => U): XTSeries[U] = new XTSeries[U](label, arr.map(f(_)))
  
	def apply(n: Int): T = arr.apply(n)
  

	def zip[U](that: XTSeries[U]): XTSeries[(T, U)] = XTSeries[(T,U)](arr.zip(that.toArray))
  
	def slice(start: Int, end: Int): XTSeries[T] = {
		require(start < arr.size && end <= arr.size && start < end, 
				s"XTSeries.slice starting $start or ending $end index incorrect")
		new XTSeries[T](label, arr.slice(start, end))
	}
  
	@inline
	final def isEmpty: Boolean = size == 0

	final def max(implicit cmp: Ordering[T]): T = arr.max
  
	
	final def min(implicit cmp: Ordering[T]): T = arr.min
  
 
	override def toString: String =  arr.mkString("\n")
  
	final val size: Int = arr.size

	def foreach( f: T => Unit) = arr.foreach(f)

	def sortWith(lt: (T,T)=>Boolean): XTSeries[T] = XTSeries[T](label, arr.sortWith(lt))
  
	def zipWithIndex: XTSeries[(T, Int)] = XTSeries[(T, Int)](label, arr.zipWithIndex)
  
	def foldLeft[U](z: U)(op: (U, T)=> U): U = arr.foldLeft(z)(op)
}


		/**
		 * <p>Class that defines a time series for multi-dimensional variables. The class is created
		 * for the purpose to encapsulate the normalization of the multi-dimensional time series.</p>
		 * @param label Name for the time series (optional)
		 * @param arr Array of values of the parameterized T
		 * @constructor Create a multidimensional time series
		 * @throws IllegalArgumentException If the array of values, arr is undefined
		 * @author Patrick Nicolas
		 * @since January, 22, 2014
		 * @note Scala for Machine Learning
		 */
@implicitNotFound("Conversion from type T to Double undefined")
final class VTSeries[T](
		label: String, 
		arr: Array[DVector[T]])
		(implicit f: T => Double) extends XTSeries[Array[T]](label, arr) {
   
		/**
		 * <p>Normalize this multi-dimensional time series
		 * @return Normalized values as an array or vectors (DblMatrix)
		 */
	def normalize(implicit ordering: Ordering[T]): DblMatrix = {
		val minMax  = toArray.map(x => (x.min, x.max))
		
		toArray.zip(minMax).map(z => { 
			val range = z._2._2 - z._2._1
			z._1.map(y => (y -z._2._1)/range)
		}) 
	}
}


		/**
		 * <p>Companion object for time series, that define constructors and most 
		 * common implicit conversions.</p>
		 * @author Patrick Nicolas
		 * @since January, 22, 2014
		 * @note Scala for Machine Learning Chapter 3 Data pre-processing / Time series
		 */
object XTSeries {  
	private val logger = Logger.getLogger("XTSeries")	
  
	type DblSeries = XTSeries[Double]
	type DblVecSeries = XTSeries[DblVector]
	
	final val EPS = 1-20
	
		/**
		 * Constructor for XTSeries with a predefined label and array of elements (or data points)
		 * @param label Name for the time series (optional)
		 * @param arr Array of values of the parameterized T
		 */
	def apply[T](label: String, arr: Array[T]): XTSeries[T] = new XTSeries[T](label, arr)
	
		/**
		 * Constructor for XTSeries with a predefined array of elements (or data points)
		 * @param arr Array of values of the parameterized T
		 */
	def apply[T](arr: Array[T]): XTSeries[T] = new XTSeries[T]("", arr)
	
		/**
		 * Constructor for XTSeries with a predefined list of elements (or data points)
		 * @param label Name for the time series (optional)
		 * @param arr Array of values of the parameterized T
		 */
	def apply[T: ClassTag](xs: List[T]): XTSeries[T] = new XTSeries[T]("", xs.toArray)

		/**
		 * Implicit conversion of a vector and label to a XTSeries
		 * @param label label for the time series
		 * @param v vector to convert 
		 */
	implicit def xTSeries[T: ClassTag](label: String, v: Vector[T]) = 
			new XTSeries[T](label, v.toArray)

		/**
		 * Implicit conversion of a list to XTSeries
		 * @param xs List to convert
		 */
	implicit def xTSeries[T: ClassTag](xs: List[T]): XTSeries[T] = 
			new XTSeries[T]("", xs.toArray)
			
		/**
		 * Implicit conversion (deep copy) of this time series
		 * @param xt Time series to duplicate
		 */
	implicit def xTseries[T](xt: XTSeries[T]) = new XTSeries[T]("", xt.toArray)
   
		/**
		 * Implicit conversion of a time series to a vector
		 * @param xt time series to convert
		 */
	implicit def series2DblVector[T](xt: XTSeries[T])(implicit f: T => Double): DblVector = 
			xt.toDblVector(f)
	
		/**
		 * Implicit conversion of a time series to Matrix of type Double
		 *  @param xt time series to convert
		 */
	implicit def series2DblMatrix[T](xt: XTSeries[T])(implicit fv: T => DblVector): DblMatrix = 
			xt.toDblMatrix(fv)
   
			/**
			 * Retrieve the dimension of the time series that is the number of variable in
			 * each feature or observations or data points
			 * @param xt time series of arrays
			 */
	def dimension[T](xt: XTSeries[Array[T]]): Int = xt.toArray(0).size
   
			/**
			 * Define an empty time series of type XTSeries
			 */
	def empty[T: ClassTag]: XTSeries[T] = new XTSeries[T]("", Array.empty[T])
   
			/**
			 * Convert a list of observations (vector) to a list of time series of these observations
			 * @param xt List of observations to convert
			 */
	def |>[T] (xs: List[Array[T]]): List[XTSeries[T]] = xs map{ XTSeries[T](_) }

		/**
		 * Implements the normalization of a parameterized time series
		 * @param xt single dimension parameterized time series
		 * @throws IllegalArgumentException if the time series is undefined
		 * @throws implicitNotFound if the implicit ordering is undefined
		 * @return normalized time series as double elements if max > min, None otherwise
		 */
	@implicitNotFound("Ordering for normalization is undefined")
	def normalize[T <% Double](xt: XTSeries[T])(implicit ordering: Ordering[T]): Option[DblSeries] = {
		require( !xt.isEmpty, "XTSeries.normalize Cannot normalize an undefined time series")
				
		val mn = xt.min
		val range = xt.max - mn
		if(range < EPS) None  else Some(xt.map(x => (x -mn)/range))
	}
   
		/**
		 * Implements the normalization of a parameterized single dimension time series within [0, 1]
		 * @param x a parameterized array
		 * @throws IllegalArgumentException if the time series is undefined
		 * @throws implicitNotFound if the implicit ordering is undefined
		 * @return normalized time series as double elements if max > min, None otherwise
		 */
	@implicitNotFound("Ordering for normalization is undefined")
	def normalize[T <% Double](x: Array[T])(implicit ordering: Ordering[T]): Option[DblVector] = {
		require( !x.isEmpty, "XTSeries.normalize  Cannot normalize an undefined time vector")
  	   
		val mn = x.min
		val range = x.max - mn
		if(range < EPS) None else Some(x.map(x => (x -mn)/range))
	}
   
		/**
		 * Implements the normalization of a parameterized multi-dimension time series within [0, 1]
		 * @param xt multi-dimension parameterized time series
		 * @throws IllegalArgumentException if the time series is undefined
		 * @return normalized time series as double elements if max > min, None otherwise
		 */
	@implicitNotFound("XTSeries.normalize Ordering for normalization is undefined")
	def normalize[T <% Double](
			xt: XTSeries[Array[T]])
			(implicit order: Ordering[T], m: Manifest[T]): Option[DblVecSeries] = {
	  require( !xt.isEmpty, 
				"XTSeries.normalize Cannot normalize an undefined time series of elements")
		require( XTSeries.dimension(xt) > 0, 
				"XTSeries.normalize Incorrect function to normalize a single dimension time series")
  	   	   
		var k = 0;
		val res = new Array[Array[T]](xt.size)
		val dimension = xt(0).size

		val min = Array.fill(dimension)( Double.MaxValue)
		val max = Array.fill(dimension)(-Double.MaxValue)
     
			// computes min and max
		while( k < xt.size) {
			var j = 0
			while( j < dimension) {
				if( xt(k)(j) < min(j)) 
					min(j) = xt(k)(j)
				else if( xt(k)(j) > max(j)) 
					max(j) = xt(k)(j)
				j += 1
			}
			k += 1
		}
		val arr = new DblMatrix(xt.size)
		k = 0
     
		Try {
			while( k < xt.size) {
				var j = 0
				arr(k) = new Array[Double](dimension)
				while( j < dimension) {
					arr(k)(j) =(xt.toArray(k)(j) - min(j))/(max(j)-min(j))
					j += 1
				}
				k += 1
			}
			new XTSeries[DblVector](xt.label, arr)
		} 
		match {
			case Success(xt) => Some(xt)
			case Failure(e) => DisplayUtils.none("XTSeries.normalize", logger, e)
		}
	}

		/**
		 * <p>transform time series of parameterized array into a array of double vector
		 * by applying the Z score transform to each element of the time series.</p>
		 * @param xt multi-dimensional parameterized time series
		 * @throws IllegalArgumentException if the time series is undefined
		 * @return Time series of double array if the function succeeds, None otherwise
		 */
	def zScoring[T <% Double](xt: XTSeries[Array[T]]): Option[XTSeries[DblVector]] = {
		require( !xt.isEmpty, "XTSeries.zScoring Cannot zScore an undefined time series")
  	 
		val stats = statistics(xt)
		var k = 0;
		val dimension = xt(0).size

		val arr = new Array[DblVector](xt.size)
		Try {
			while( k < xt.size) {
				var j = 0
				arr(k) = new Array[Double](dimension)
				while( j < dimension) {
					arr(k)(j) =(xt.toArray(k)(j) - stats(j).mean)/stats(j).stdDev
					j += 1
				}
				k += 1
			}
			new XTSeries[DblVector](xt.label, arr)
		} 
		match {
			case Success(xt) => Some(xt)
			case Failure(e) => DisplayUtils.none("XTSeries.zScoring", logger, e)
		}
	}
 
		/**
		 * Transpose an array of array of data
		 * @param from Array of Array to convert
		 */
	def transpose[T](from: Array[Array[T]]): Array[Array[T]] = from.transpose
 
		/**
		 * Transpose a list of array into an array of array
		 * @param from List of observations to transpose
		 */
	def transpose[T: ClassTag](from: List[Array[T]]):  Array[Array[T]] = from.toArray.transpose
 
		/**
		 * Compute the basic aggregate statistics for a time series
		 * @param xt time series for which the statistics are computed
		 * @return Statistics instance
		 */
	def statistics[T <% Double](xt: XTSeries[T]): Stats[T] = Stats[T](xt.toArray)

			/**
		 * Compute the basic statistics for each dimension of a time series
		 * @param xt time series for which the statistics are computed
		 * @return Array of statistics for each dimension
		 */
	def statistics[T <% Double](xt: XTSeries[Array[T]]): Array[Stats[T]] = {
		require( !xt.isEmpty, "XTSeries.statistics input time series undefined")
      
		import Stats._
		val transposed = xt.toArray.transpose
		val results = transposed.map(Stats[T]( _ ))
		results
	}
}

// ---------------------------------  EOF --------------------------------------------------------
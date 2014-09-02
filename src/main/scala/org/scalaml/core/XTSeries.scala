/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.core

import scala.reflect.ClassTag
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.stats.Stats
import scala.Array.canBuildFrom
import scala.annotation.implicitNotFound
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display


		/**
		 * <p>Generic class for time series. Any type from different libraries are converted into 
		 * this generic type to avoid multiple conversion between numerous types.<br>
		 * The class is parameterized so it can take primitive types to create vector for single
		 * variable time series or arrays/list to create matrix for multiple variables time series. </p>
		 * @param label optional name for the time series
		 * @param arr array of values of parameterized types
		 * @throws IllegalArgumentException if the array of values, arr is undefined
		 * 
		 * @author Patrick Nicolas
		 * @since January, 22, 2014
		 * @note Scala for Machine Learning
		 */
class XTSeries[@specialized(Double) T](val label: String, protected val arr: Array[T]) { 
  require(arr != null && arr.size > 0, "Cannot create a times series from undefined values")
  
  def toArray: Array[T] = arr
  def toList: List[T] = arr.toList
  
  @inline def head: T = arr.head
	
  @inline def last: T = arr.last
  
  	/**
  	 * Test if a time series is identical to this time series. The label is not included
  	 * in the comparison.
  	 * @param that other series  this series is compared to
  	 * @throws IllegalArgumenException if the argument is null
  	 * @return true if the series are identical, false if the other time series, 'that' is null or is different from this time series
  	 */
  def == (that: XTSeries[T]): Boolean = {
  	require(that != null, "Cannot compare this time series with undefined time series")
  	
  	if(that != null) size == that.size && arr.equals(that.toArray) else false
  }
        
  	/**
  	 * <p>Convert a this time series into a vector of Double floating point values.</p>
  	 * @param f  implicit conversion of type T to Double
  	 * @return Vector of double values (DblVector)
  	 * @throws IllegalArgumentException if the implicit conversion T to DblMatrix is explicitly provided as a null function
  	 * @throws implicitNotFound if the implicit conversion is undefined
  	 */
  @implicitNotFound("Conversion from type T to DblVector undefined")
  def toDblVector(implicit f: T => Double): DblVector = {
  	require( f != null, "Cannot convert the time series to dblVector with undefined conversion")
  	arr.map( f( _ ))
  }
  
    /**
  	 * <p>Convert a this time series into a matrix of Double floating point values.</p>
  	 * @param f  implicit conversion of type T to DblVector
  	 * @return Matrix of double values (DblMatrix)
  	 * @throws IllegalArgumentException if the implicit conversion T to DblVector is explicitly provided as a null function.
  	 * @throws implicitNotFound if the implicit conversion is undefined
  	 */
  @implicitNotFound("Conversion from type T to DblMatrix undefined")
  def toDblMatrix(implicit fv: T => DblVector): DblMatrix = {
  	 require( fv != null, "Cannot convert the time series to matrix DblMatrix with undefined conversion")
  	 arr.map( fv( _ ) )
  }
  
  def + (n: Int, t: T)(implicit f: (T,T) => T): T = f(arr(n), t)
  
  @inline
  def tail: XTSeries[T] = new XTSeries(label, arr.tail)
  
  def take(n: Int): XTSeries[T] = new XTSeries(label, arr.take(n))
  def takeRight(n: Int): XTSeries[T] = new XTSeries(label, arr.takeRight(n))
  
  def drop(n: Int):  XTSeries[T] = new XTSeries(label, arr.drop(n))
  
  def map[U: ClassTag](f: T => U): XTSeries[U] = new XTSeries[U](label, arr.map(f(_)))
  
  def apply(n: Int): T = arr.apply(n)
  
  @inline
  def zip[U](that: XTSeries[U]): XTSeries[(T, U)] = XTSeries[(T,U)](arr.zip(that.toArray))
  
  def slice(start: Int, end: Int): XTSeries[T] = {
    require( start < arr.size && end < arr.size && start < end, "Slice of XTSeries is incorrectly specified")
    new XTSeries[T](label, arr.slice(start, end))
  }
  
  @inline 
  def max(implicit cmp: Ordering[T]): T = arr.max
  
  @inline 
  def min(implicit cmp: Ordering[T]): T = arr.min
  
 
  override def toString: String =  arr.foldLeft(new StringBuilder)((b, x) => b.append(x).append("\n") ).toString
  
  @inline 
  def size: Int = arr.size

  def foreach( f: T => Unit) = arr.foreach(f)
  
  def sortWith(lt: (T,T)=>Boolean): XTSeries[T] = XTSeries[T](label, arr.sortWith(lt))
  
  def zipWithIndex: XTSeries[(T, Int)] = XTSeries[(T, Int)](label, arr.zipWithIndex)
  
  def foldLeft[U](z: U)(op: (U, T)=> U): U = arr.foldLeft(z)(op)
}



		/**
		 * <p>Class that defines a time series for multi-dimensional variables. The class is created
		 * for the purpose to encapsulate the normalization of the multi-dimensional time series.</p>
		 * @param _label label for the multi-dimensional time series
		 * @param _arr Array of vectors
		 * 
		 */
@implicitNotFound("Conversion from type T to Double undefined")
class VTSeries[T](val _label: String, val _arr: Array[DVector[T]])(implicit val f: T => Double) extends XTSeries[Array[T]](_label, _arr) {
   
  def normalize(implicit ordering: Ordering[T]): DblMatrix = {
     val minMax = arr.map(x => (x.min, x.max))
     arr.zip(minMax).map(z => { 
        val range = z._2._2 - z._2._1
        z._1.map( y => (y -z._2._1)/range)
     }) 
  }
}



		/**
		 * <p>Companion object for time series, that define constructors and most 
		 * common implicit conversions.
		 */
object XTSeries {  
  private val logger = Logger.getLogger("XTSeries")	
	
   final val EPS = 1-20
   def apply[T](label: String, arr: Array[T]): XTSeries[T] = new XTSeries[T](label, arr)
   def apply[T](arr: Array[T]): XTSeries[T] = new XTSeries[T]("", arr)
   def apply[T : ClassTag](xs: List[T]): XTSeries[T] = new XTSeries[T]("", xs.toArray)
      
   implicit def xTSeries[T: ClassTag](label: String, v: Vector[T]) = new XTSeries[T](label, v.toArray)
   implicit def xTSeries[T: ClassTag](xs: List[T]): XTSeries[T] = new XTSeries[T]("", xs.toArray)
   implicit def xTSeries[T: ClassTag](xs: Array[String])(implicit fc: String=> T) = new XTSeries[T]("", xs.toArray.map{fc(_)})
   implicit def xTseries[T](xtseries: XTSeries[T]) = new XTSeries[T]("", xtseries.arr)
   
   implicit def series2DblVector[T](series: XTSeries[T])(implicit f: T => Double): DblVector = series.toDblVector(f)
   implicit def series2DblMatrix[T](series: XTSeries[T])(implicit fv: T => DblVector): DblMatrix = series.toDblMatrix(fv)
   
   def dimension[T](xt: XTSeries[Array[T]]): Int = xt.toArray(0).size
   
   
   def |>[T] (xs: List[Array[T]]): List[XTSeries[T]] = xs map{ XTSeries[T](_) }

   		/**
   		 * Implements the normalization of a parameterized single dimension time series within [0, 1]
   		 * @param xt single dimension parameterized time series
   		 * @throws IllegalArgumentException if the time series is undefined
   		 * @return normalized time series as double elements if max > min, None otherwise
   		 */
   @implicitNotFound("Ordering for normalizatoin is undefined")
   def normalize[T <% Double](xt: XTSeries[T])(implicit ordering: Ordering[T]): Option[XTSeries[Double]] = {
  	   require(xt != null, "Cannot normalize an undefined time series of elements")
  	   val mn = xt.min
       val range = xt.max - mn
       if(range < EPS) None  else Some(xt.map(x => (x -mn)/range))
   }
   
   @implicitNotFound("Ordering for normalizatoin is undefined")
   def normalize[T <% Double](feature: Array[T])(implicit ordering: Ordering[T]): Option[DblVector] = {
  	   require(feature != null, "Cannot normalize an undefined time vector")
  	   
  	   val mn = feature.min
       val range = feature.max - mn
       if(range < EPS) None  else Some(feature.map(x => (x -mn)/range))
   }
   
   
   
         /**
   		 * Implements the normalization of a parameterized multi-dimension time series within [0, 1]
   		 * @param xt multi-dimension parameterized time series
   		 * @throws IllegalArgumentException if the time series is undefined
   		 * @return normalized time series as double elements if max > min, None otherwise
   		 */
   @implicitNotFound("Ordering for normalizatoin is undefined")
   def normalize[T <% Double](xt: XTSeries[Array[T]])(implicit order: Ordering[T], m: Manifest[T]): Option[XTSeries[DblVector]] = {
  	 require(xt != null && xt.size > 0, "Cannot normalize an undefined time series of elements")
  	 require(xt(0).size > 0, "Incorrect function to normalize a single dimension time series")
  	   	   
     var k = 0;
     val res = new Array[Array[T]](xt.size)
     val dimension = xt(0).size

     val min = Array.fill(dimension)( Double.MaxValue)
     val max = Array.fill(dimension)(- Double.MaxValue)
     
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
	    		arr(k)(j) =(xt.arr(k)(j) - min(j))/(max(j)-min(j))
	    		j += 1
	    	}
	    	k += 1
	     }
	  	 new XTSeries[DblVector](xt.label, arr)
     } match {
    	 case Success(xt) => Some(xt)
    	 case Failure(e) => Display.error("Normalize ", logger, e); None
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
  	 require(xt != null && xt.size > 0, "Cannot zScore an undefined time series of elements")
  	 
  	  val stats = statistics(xt)
  	  var k = 0;
  	  val dimension = xt(0).size
  	       
  	  val arr = new Array[DblVector](xt.size)
  	  Try {
	      while( k < xt.size) {
	    	var j = 0
	    	arr(k) = new Array[Double](dimension)
	    	while( j < dimension) {
	    		arr(k)(j) =(xt.arr(k)(j) - stats(j).mean)/stats(j).stdDev
	    		j += 1
	    	}
	    	k += 1
	     }
	     new XTSeries[DblVector](xt.label, arr)
  	  } match {
  	  	case Success(xt) => Some(xt)
  	  	case Failure(e) => Display.error("zScoring ", logger, e); None
  	  }
   }
   
   def transpose[T](from: Array[Array[T]]): Array[Array[T]] = from.transpose
   
   def transpose[T: ClassTag](from: List[Array[T]]):  Array[Array[T]] = from.toArray.transpose
   
   
   def statistics[T <% Double](xt: XTSeries[T]): Stats[T] = Stats[T](xt.arr)

   def statistics[T <% Double](xt: XTSeries[Array[T]]): Array[Stats[T]] = {
       import Stats._
       xt.arr.transpose.map( new Stats[T]( _ ))
   }
}


// ---------------------------------  EOF --------------------------------------------------------
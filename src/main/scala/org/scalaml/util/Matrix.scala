/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.util

import scala.reflect.ClassTag
import scala.util.Random
import org.scalaml.core.Types.ScalaMl._


		/**
		 *  @author Patrick Nicolas
		 *  @since Feb 23, 2014
		 *  @note Book
		 */

class Matrix[@specialized(Double, Int) T: ClassTag](val nRows: Int, val nCols: Int, val data: Array[T])(implicit f: T => Double) {

   
 //  def this(xy: Array[Array[T]])(implicit f: T => Double) = this(xy.size, xy(0).size, xy.flatten)
 //  def this(nCols: Int, nRows: Int)(implicit f: T => Double) = this(nCols, nRows, new Array[T](nCols*nRows))
  
   def apply(i: Int, j: Int): T = data(i*nCols+j)
   def += (i: Int, j : Int, t: T): Unit = data(i*nCols +j) = t
   
   def diff(that: Matrix[T], error: (T, T) => Double)(implicit f: T => Double): Double = {
  	  require( nCols == that.nCols && nRows == that.nRows, "cannot compare Matrices of different sizes")
  	  
  	  data.zip(that.data).foldLeft(0.0)((s, xy) => s + error(xy._1, xy._2))
   }
   
   def cols(i: Int): Array[T] = { val idx = i*nRows; data.slice(idx, idx + nRows) }
   
   def /= (i: Int, t: T)(implicit g: Double => T): Unit =  {
  	  var k = 0
  	  while(k < nRows ) {
  	     data(i*nRows +k) /= t
  	     k += 1
  	  }
   }
   
   def += (i: Int, t: T): Unit = {
  	  var k = 0
  	  while(k < nRows ) {
  	     data(i*nRows +k) = t
  	     k += 1
  	  }
   }
   
   
   def fillRandom(mean: Double)(implicit f: Double =>T): Unit = {
      var k = 0
      val rGen = new Random(System.currentTimeMillis)
      while( k < data.size) {
        data(k) = f(mean*rGen.nextDouble)
        k += 1
      }
   }

   override def toString: String = {
      var count = 0
      data.foldLeft(new StringBuilder)((b, x) => { 
         count += 1; 
         b.append(x).append(  if( count % nCols == 0) "\n" else ",")
      }).toString
   }
}


object Matrix {
   import org.apache.commons.math3.linear.{Array2DRowRealMatrix, RealMatrix }
   implicit def matrix2RealMatrix(m: Matrix[Double]): RealMatrix = new Array2DRowRealMatrix(m.data)
   
   def apply[T: ClassTag](nRows: Int, nCols: Int, data: Array[T])(implicit f: T => Double): Matrix[T] = new Matrix(nRows, nCols, data)
   def apply[T: ClassTag](nRows: Int, nCols: Int)(implicit f: T => Double): Matrix[T] = new Matrix(nRows, nCols, new Array[T](nCols*nRows))
   def apply[T: ClassTag](xy: Array[Array[T]])(implicit f: T => Double): Matrix[T] = new Matrix(xy.size, xy(0).size, xy.flatten)
}



// ------------------------------------  EOF ---------------------------------------------
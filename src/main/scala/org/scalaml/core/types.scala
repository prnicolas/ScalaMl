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
package org.scalaml.core

import scala.language.implicitConversions
import scala.util.Try
	
		/**
		 * Package that encapsulates the types and conversion used in the Scala for Machine learning
		 * project. Internal types conversion are defined by the Primitives singleton. The type 
		 * conversion related to each specific libraries are defined by their respective singleton 
		 * (i.e CommonsMath).
		 * 
		 *  @author Patrick Nicolas
		 *  @since February 23, 2014 0.98
		 *  @version 0.98.1
		 *  @see Scala for Machine Learning Chapter 3 Data pre-processing/Time series
		 */
object Types {
  

		/**
		 * Singleton that define the Scala types and their conversion to native Scala types
		 *  @author Patrick Nicolas
		 *  @since February 23, 2014 0.98
		 *  @version 0.98.1
		 *  @see Scala for Machine Learning Chapter 3 Data pre-processing/Time series
		 */
	object ScalaMl {	  
		type DblPair = (Double, Double)
		type DblMatrix = Array[DblArray]
		type DblArray = Array[Double]
		type DblVector = Vector[Double]
		type DblPairVector = Vector[DblPair]
		
		type XSeries[T] = Vector[T]
		type XVSeries[T] = Vector[Array[T]]

		case class Pair(p: DblPair) {
		   def + (o: Pair): Pair = Pair((p._1 + o.p._1, p._2 + o.p._2))
		   def / (o: Pair): Pair = Pair((p._1/o.p._1, p._2/o.p._2))
		}

			
		final def sqr(x: Double): Double = x*x

			/**
			 * Default conversion from Int to Double
			 * @param n  Integer input
			 * @return Double version of the integer
			 */
		implicit def intToDouble(n: Int): Double = n.toDouble
	 
		import scala.reflect.ClassTag
		implicit def t2Array[T: ClassTag](t: T): Array[T] =  Array.fill(1)(t)
		implicit def arrayT2DblArray[T <: AnyVal](vt: Array[T])(implicit f: T => Double): DblArray = 
			vt.map( _.toDouble)


			/**
			 * In place division of all elements of a given row of a matrix
			 * @param m Matrix of elements of type Double
			 * @param row  Index of the row
			 * @param z  Quotient for the division of elements of the given row
			 * @throws java.lang.IllegalArgumentException If the row index is out of bounds
			 */
		@throws(classOf[IllegalArgumentException])
		implicit def /(m: DblMatrix, row: Int, z: Double): Unit  = {
			require(row < m.length, s"/ matrix column $row out of bounds")
			require(Math.abs(z) > 1e-32, s"/ divide column matrix by $z too small")
			
			m(row).indices.foreach( m(row)(_) /= z)
		}

			/**
			 * Implicit conversion of a XVSeries[T] to a Matrix of Double
			 * @param xt  Time series of elements Array[T]
			 * @param f  Implicit conversion of element (or data point) of type T to Double
			 * @tparam T  Type of elements of feature
			 * @return Matrix of type Array[Array[Double]]
			 */
		implicit def seriesT2Double[T <: AnyVal](xt: XVSeries[T])(implicit f: T => Double): DblMatrix = 
				xt.map( _.map(_.toDouble)).toArray

			/**
			 * Implicit conversion of a pair of pairs to a Matrix with elements of type Double
			 * @param x  Pair of tuple (Double, Double)
			 * @return 2x2 matrix of elements of type Double
			 */
		implicit def dblPairs2DblMatrix2(x: ((Double, Double), (Double, Double))): DblMatrix = 
				Array[DblArray](Array[Double](x._1._1, x._1._2), Array[Double](x._2._1, x._2._2))

		implicit def /(v: DblArray, n: Int): Try[DblArray] = Try(v.map( _/n))
	
		/**
		 * Textual representation of a vector with and without the element index
		 * @param v vector to represent
		 * @param index flag to display the index of the element along its value. Shown if index is 
		 * true, not shown otherwise
		 */
		@throws(classOf[IllegalArgumentException])
		def toText(v: DblArray, index: Boolean): String = {
			require( v.length > 0, 
						"ScalaMl.toText Cannot create a textual representation of a undefined vector")
			
			if( index)  v.zipWithIndex.map{ case(x ,n) => s"$x:$n"}.mkString(", ")
			else  v.mkString(", ").trim
		}

		/**
		 * Textual representation of a matrix with and without the element index
		 * @param m matrix to represent
		 * @param index flag to display the index of the elements along their value. Shown if 
		 * index is true, not shown otherwise
		 */
		@throws(classOf[IllegalArgumentException])
		def toText(m: DblMatrix, index: Boolean): String = {
			require( m.length > 0, 
					"ScalaMl.toText Cannot create a textual representation of a undefined vector")
			
			if(index)
			  m.zipWithIndex.map{ case(v, n) => s"$n:${toText(v, index)}"}.mkString("\n")
			else 
			  m.map(v => s"${toText(v, index)}").mkString("\n")
		}
	}

	val emptyString = ""
}

// ------------------------------------------  EOF -----------------------------------------------------------
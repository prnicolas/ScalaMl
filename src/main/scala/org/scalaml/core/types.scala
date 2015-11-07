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

		case class Pair(val p: DblPair) {
		   def + (o: Pair): Pair = Pair((p._1 + o.p._1, p._2 + o.p._2))
		   def / (o: Pair): Pair = Pair((p._1/o.p._1, p._2/o.p._2))
		}

			
		final def sqr(x: Double): Double = x*x
		
		implicit def intToDouble(n: Int): Double = n.toDouble
	 
		import scala.reflect.ClassTag
		implicit def t2Array[T: ClassTag](t: T): Array[T] =  Array.fill(1)(t)
		implicit def arrayT2DblArray[T <: AnyVal](vt: Array[T])(implicit f: T => Double): DblArray = 
			vt.map( _.toDouble)

			
		
		@throws(classOf[IllegalArgumentException])
		implicit def /(m: DblMatrix, n: Int, z: Double): Unit  = {
			require(n < m.length, s"/ matrix column $n out of bounds")
			require(Math.abs(z) > 1e-32, s"/ divide column matrix by $z too small")
			
			Range(0, m(n).size).foreach( m(n)(_) /= z)
		}

		implicit def seriesT2Double[T <: AnyVal](xt: XVSeries[T])(implicit f: T => Double): DblMatrix = 
				xt.map( _.map(_.toDouble)).toArray
		
		
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
			
			if( index)
			  v.zipWithIndex.map{ case(x ,n) => s"${x}:${n}"}.mkString(", ")

			else
			  v.mkString(", ").dropRight(1)
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
			  m.zipWithIndex.map{ case(v, n) => s"$n:${toText(v, true)}"}.mkString("\n")

			else 
			  m.map(v => s"${toText(v, false)}").mkString("\n")
		}
	}


	val emptyString = ""
}

// ------------------------------------------  EOF -----------------------------------------------------------
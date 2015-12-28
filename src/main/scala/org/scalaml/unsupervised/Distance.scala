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
package org.scalaml.unsupervised


		/**
		 * Singleton which defines the different distances used by unsupervised machine learning 
		 * techniques.
		 * @author Patrick Nicolas
		 * @since 0.98 February 16, 2014
		 * @version 0.99.1
		 * @see Scala for Machine Learning Chapter 4 "Unsupervised Learning" Measuring similarity
		 * @note replace ''/:'' by ''aggregate''
		 */
object Distance {
  private final val sqr = (x :Double) => x*x
	
		/**
		 * Function that compute the Manhattan distance between two
		 * array (vectors ) of values.
		 * @tparam T type of elements of the 1st vector used in the computation of the distance
		 * @tparam U type of elements of the 2nd vector used in the computation of the distance
		 * @param x first array/vector/data point
		 * @param y second array/vector/data point
		 * @throws IllegalArgumentException if the input array are undefined or have different size
		 * @return distance between two data points
		 */
  @throws(classOf[IllegalArgumentException])
	def manhattan[T, U](x: Array[T], y: Array[U])
			(implicit f: T => Double, g: U => Double): Double = {
		require( x.length == y.length, 
				s"Distance.manhattan Vectors have different size ${x.length} and ${y.length}")
				
		(x,y).zipped.map{ case (u,v) => Math.abs(u-v)}.sum
	}
  
		/**
		 * Function that compute the Euclidean distance between two
		 * array (vectors ) of values.
		 * @tparam T type of elements of the 1st vector used in the computation of the distance
		 * @tparam U type of elements of the 2nd vector used in the computation of the distance
		 * @param x first array/vector/data point
		 * @param y second array/vector/data point
		 * @throws IllegalArgumentException if the input array are undefined or have different size
		 * @return distance between two data points
		 */
  @throws(classOf[IllegalArgumentException])
	def euclidean[T, U](x: Array[T], y: Array[U])
			(implicit f: T => Double, g: U => Double): Double = {
		require( x.length == y.length, 
				s"Distance.euclidean Vectors have different size ${x.length} and ${y.length}")

		Math.sqrt((x, y).zipped.map{case (u,v) => u-v}.map(sqr(_)).sum)
	}

		/**
		 * Function that compute the normalized inner product or cosine distance between two
		 * vectors
		 * @tparam T type of elements of the 1st vector used in the computation of the distance
		 * @tparam U type of elements of the 2nd vector used in the computation of the distance
		 * @param x first array/vector/data point
		 * @param y second array/vector/data point
		 * @throws IllegalArgumentException if the input array are undefined or have different size
		 * @return distance between two data points
		 */
  @throws(classOf[IllegalArgumentException])
	def cosine[T, U](x: Array[T], y: Array[U])
			(implicit f: T => Double, g: U => Double): Double = {
		require( x.length == y.length,
			s"Distance.cosine Vectors have different size ${x.length} and ${y.length}")
 
		val norms = (x,y).zipped.map{case (u,v) => Array[Double](u*v, u*u, v*v) }
				./:(Array.fill(3)(0.0))((s, t) => s ++ t)

		norms(0)/Math.sqrt(norms(1)*norms(2))
	}
}

// -------------------------------  EOF ----------------------------------------------------
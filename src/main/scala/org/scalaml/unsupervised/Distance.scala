/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.unsupervised


		/**
		 * <p>Singleton which defines the different distances used by 
		 * unsupervised machine learning techniques.</p>
		 * @author Patrick Nicolas
		 * @since February 16, 2014
		 * @note Scala for Machine Learning Chapter 4 Unsupervised Learning / Measuring similarity
		 */
object Distance {
	
		/**
		 * <p>Function that compute the Manhattan distance between two
		 * array (vectors ) of values.</p>
		 * @param x first array/vector/data point
		 * @param y second array/vector/data point
		 * @throws IllegalArgumentException if the input array are undefined or have different size
		 * @return distance between two data points
		 */
	def manhattan[T <% Double, U <% Double](x: Array[T], y: Array[U]): Double = {
		require( !x.isEmpty && !y.isEmpty, 
			"Distance.manhattan Input data points vectors are undefined")
		require( x.size == y.size, 
				s"Distance.manhattan Vectors have different size ${x.size} and ${y.size}")
				
		(x, y).zipped.foldLeft(0.0)((s, t) => s + Math.abs(t._1 - t._2))
	}
  
		/**
		 * <p>Function that compute the Euclidean distance between two
		 * array (vectors ) of values.</p>
		 * @param x first array/vector/data point
		 * @param y second array/vector/data point
		 * @throws IllegalArgumentException if the input array are undefined or have different size
		 * @return distance between two data points
		 */
	def euclidean[T <% Double, U <% Double](x: Array[T], y: Array[U]): Double = {
		require( !x.isEmpty && !y.isEmpty, 
				"Distance.euclidean Input data points vectors are undefined")
		require( x.size == y.size, 
				s"Distance.euclidean Vectors have different size ${x.size} and ${y.size}")

		Math.sqrt((x, y).zipped.foldLeft(0.0)((s, t) => { val d = t._1 - t._2; s + d*d} )) 
	}

		/**
		 * <p>Function that compute the Cosine distance between two
		 * array (vectors ) of values.</p>
		 * @param x first array/vector/data point
		 * @param y second array/vector/data point
		 * @throws IllegalArgumentException if the input array are undefined or have different size
		 * @return distance between two data points
		 */
	def cosine[T <% Double, U <% Double](x: Array[T], y: Array[U]): Double = {
		require( !x.isEmpty && !y.isEmpty, 
			"Distance.cosine Input data points vectors are undefined")
		require( x.size == y.size,
			"Distance.cosine Vectors have different size ${x.size} and ${y.size}")
 
		val zeros = (0.0, 0.0, 0.0)
		val norms = (x, y).zipped.foldLeft(zeros)((s, t) => 
				(s._1 + t._1*t._2, s._2 + t._1*t._1, s._3 + t._2*t._2))
		norms._1/Math.sqrt(norms._2*norms._3)
	}
}

// -------------------------------  EOF ----------------------------------------------------
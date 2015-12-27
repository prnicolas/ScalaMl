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
package org.scalaml.util


import org.scalaml.core.Types.ScalaMl._
import org.scalaml.stats.XTSeries
import XTSeries._


	/**
	 * Singleton that define the comparison between data sets with different types
	 * 
	 * @author Patrick Nicolas
	 * @since 0.99 June 13, 2015
	 */
trait Assertable {
	protected val assertMsg: String

		/**
		 * Method that compare multi-dimensional data set of type XVSeries
		 * @param predicted Predicted or computed values to be evaluated
		 * @param expected Expected value or labels to be compared with
		 * @param eps error criteria
		 * @throws IllegalArgumentException if the input time series have different size
		 * @return true if each element of the two time series are similar within the error range
		 * @note The ''IllegalArgumentException'' is thrown by '''zipToXVSeries''' XTSeries method
		 */
	@throws(classOf[IllegalArgumentException])
	protected def assertXVSeries(
			predicted: XVSeries[Double], 
			expected: XVSeries[Double], 
			eps: Double): Unit = {
	  
		val xCompare = (x: Double, y: Double) => Math.abs(x-y)
		val fCompare = (x: DblArray, y: DblArray) => {
		  val aa = zipToArray(x, y)(xCompare)
		  aa.sum
		}
		
		assert( !zipToXVSeries(predicted, expected)(fCompare).exists( _ > eps ), assertMsg)
	}
	
		/**
		 * Method that compare one-dimensional data set of type XVSeries
		 * @param predicted Predicted or computed values to be evaluated
		 * @param expected Expected value or labels to be compared with
		 * @param eps error criteria
		 * @throws IllegalArgumentException if the input time series have different size
		 * @return true if each element of the two time series are similar within the error range
		 * @note The ''IllegalArgumentException'' is thrown by '''zipToXSeries''' XTSeries method
		 */	
	@throws(classOf[IllegalArgumentException])
	protected def assertXSeries(
			predicted: XSeries[Double], 
			expected: XSeries[Double], 
			eps: Double): Int = {
	  
		val xCompare = (x: Double, y: Double) => Math.abs(x-y)
		assert( !zipToXSeries(predicted, expected)(xCompare).exists( _ > eps ), assertMsg)
		1
	}
	
			/**
		 * Method that compare one-dimensional data set of type XVSeries
		 * @param predicted Predicted or computed values to be evaluated
		 * @param expected Expected value or labels to be compared with
		 * @param eps error criteria
		 * @throws IllegalArgumentException if the input time series have different size
		 * @return true if each element of the two time series are similar within the error range
		 * @note The ''IllegalArgumentException'' is thrown by '''zipToXSeries''' XTSeries method
		 */	
	@throws(classOf[IllegalArgumentException])
	protected def assertDblArray(
			predicted: Array[Double], 
			expected: Array[Double], 
			eps: Double): Int = assertXSeries(predicted.toVector, expected.toVector, eps)
			
		/**
		 * Method that compares two values of type Double
		 * @param predicted Predicted or computed value
		 * @param expected Expected value
		 * @param eps error criteria
		 * @return true if the value are similar within the error range
		 */	
	protected def assertDouble(predicted: Double, expected: Double, eps: Double): Unit = {
		assert( Math.abs(predicted - expected) < eps, assertMsg)
	}
	
			/**
		 * Method that compares two values of type Int
		 * @param predicted Predicted or computed value
		 * @param expected Expected value
		 * @return true if the values are equals
		 */	
	protected def assertInt(predicted: Int, expected: Int): Unit = {
		assert( predicted == expected, assertMsg)
	}
	
	protected def assertVector[T](predicted: Vector[T], expected: Vector[T]): Unit = {
		val failed = !(!predicted.zip(expected.view).forall { case (p, e) => p == e })
		assert( failed, assertMsg)
	}
		
	protected def assertList[T](predicted: List[T], expected: List[T]): Unit = 
		assertVector(predicted.toVector, expected.toVector)

	
	protected def assertT[T](predicted: T, expected: T): Unit = {
		assert( predicted == expected, assertMsg)
	}
}

// ---------------------------- EOF ----------------------
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
package org.scalaml.stats


import org.scalaml.core.Types.ScalaMl._
  import scala.util.Try

	/**
	 * Generic trait for the computation of difference between time series. This
	 * trait is part of the Magnet pattern used to support any kind of differential
	 * computation on time series, without regard of the type of data point of the
	 * result of the computation. A difference is computed as
	 * {{{
	 * 	dx/dt ~ x(t) -x(t-1) for differential of step 1 
	 *  dx/dt ~ x(t) - x(t-p) for differential of step p
	 * }}}
	 * 
	 * @author Patrick Nicolas
	 * @version 0.99
	 * @see org.scalaml.supervised.MultilinearRegression
	 * @see Scala for Machine Learning Chap 3, ''Data Pre-processing'' / Time series in Scala
	 */
sealed trait Difference[T] {
		/**
		 * Overriden return time
		 */
	type Result
		/**
		 * Executes the computation of the difference
		 * @return Polymorphic difference of time series
		 */
	def apply(): Result
}


		/**
		 * Object companion to the trait ''Difference''. Its purpose is to implement a generic
		 * Magnet pattern to support any kind of differential computation of data sets or time
		 * series.
		 * 
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see Scala for Machine Learning Chap 3, ''Data Pre-processing'' / Time series in Scala
		 */
object Difference {
	import language.implicitConversions

	final val diffDouble = (x: Double, y: Double) => y -x	
	final val diffInt = (x: Double, y: Double) => if(y > x) 1 else 0
	final val diffBoolean = (x: Double, y: Double) => if(y > x) true else false
  
	
		/**
		 * Implement the computation of the difference for single variable
		 * time series x(t) - x(t-10
		 * @param valuesf Tuple (input time series of type ''Vector[Double]'', reducing function 
		 * with a return type T)
		 * @return Time series of type Vector[T]
		 * @see org.scalaml.stats.XTSeries.zipWithShift1
		 */
	implicit def vector2Double[T](valuesf: (DblVector, (Double, Double) => T)) = new Difference[T] {
		type Result = Vector[T]
		def apply(): Result = 
			XTSeries.zipWithShift1(valuesf._1).collect { case( next, prev) => valuesf._2(prev, next) }
	}
   
		/**
		 * Implement the computation of the difference for a time series of multivariable variables 
		 * (array xt)  xt(t) - xt(t-1)
		 * @param valuesf Tuple (input time series of type ''Vector[Array[Double]]'', reducing function 
		 * for features of type ''Array[Double]'')
		 * @return Time series of type ''Vector[Array[T]]''
		 * @see org.scalaml.stats.XTSeries.zipWithShift1
		 */
	implicit def vector2Series[T](valuesf: (XVSeries[Double], (DblArray, DblArray) => Array[T])) = new Difference[T] {
		type Result = Vector[Array[T]]
			def apply(): Result = 
			XTSeries.zipWithShift1(valuesf._1).collect { case( next, prev) => valuesf._2(prev, next) }
	}

		/**
		 * Generic computation of the difference for any kind of time series
		 * @param diff Difference to be processed
		 * @return A time series of type ''Difference#Result''
		 */
	def difference[T](diff: Difference[T]): diff.Result = diff()
  
  
			/**
			 * Helper method to compute the differential of a target, single variable times series
			 * @param x input time series
			 * @param target Time series to be differentiated
			 * @param reducing or differential function
			 * @return Try wrapper for the differential time series
			 */
	def differentialData[T](
			x: DblVector, 
			target: DblVector,
			f: (Double, Double) =>T): Try[(DblVector, Vector[T])] = Try((x, difference(target, f)))
			
			
			/**
			 * Helper method to compute the differential of a target, single variable times series.
			 * The method zips the two input time series
			 * @param x First input time series
			 * @param y Second input time series
			 * @param target Time series to be differentiated
			 * @param reducing or differential function
			 * @return Try wrapper for the differential time series
			 */
	def differentialData[T](
			x: DblVector, 
			y: DblVector, 
			target: DblVector,
			f: (Double, Double) =>T): Try[(XVSeries[Double], Vector[T])] = 
	  Try((XTSeries.zipToSeries(x, y, 1), difference((target, f))))
}


// -----------------------------  EOF --------------------
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
package org.scalaml.workflow.module

import scala.util.Try
import scala.language.implicitConversions

import org.scalaml.stats.XTSeries
import org.scalaml.filtering.movaverage.ExpMovingAverage
import org.scalaml.core.Types.ScalaMl._
import org.apache.log4j.Logger
import org.scalaml.util.DisplayUtils

import DisplayUtils._


	/**
	 * Preprocessing module used to instantiate a pre-processing/filtering component 
	 * in a workflow. The module can contain an arbitrary number of filtering algorithms. This
	 * class illustrates the injection dependency capabilities of Scala
	 * @tparam T type of input values
	 * @author Patrick Nicolas
	 * @since 0.98  (January 23, 2014)
	 * @version 0.98.2
	 * @see Scala for Machine Learning Chapter 2 "Hello World!" Designing a workflow, Modularization
	 */
trait PreprocessingModule[T] {
	private val logger = Logger.getLogger("PreprocessingModule")
	
	implicit val convert = (t: T) => Double
  
		/**
		 * Pre-processing algorithm to be defined at run-time
		 */
	val preprocessor: Preprocessor[T]
		/**
		 * Base class for all pre-processing algorithms
		 */
	trait Preprocessor[U] {
		/**
		 * Pre-process a time series using this specific filtering algorithm
		 * @param xt vector of input data 
		 * @return A vector of Double values if computation is successful, None otherwise
		 */
		def execute(xt: Vector[U]): Try[DblVector]
	}


		/**
		 * Wrapper for the exponential moving average defined in Chapter 3 "Data pre-processing"
		 * @author Patrick Nicolas
		 * @since 0.98.1
		 * @version 0.99.1
		 * 
		 * @tparam  T type of single feature input data bounded (view) by Double
		 * @constructor Create an exponential moving average wrapper 
		 * @param period Period or size fo the time window in the moving average
		 * @throws IllegalArgumentException if period is non positive or alpha is out of range [0,1]
		 * @see Scala for machine learning Chapter 2 "Hello World!" modularization
		 * @see org.scalaml.filtering.ExpMovingAverage
		 */
	final class ExpMovingAverage[U <: AnyVal](period: Int)(implicit num: Numeric[T], f: U=> Double)
			extends Preprocessor[U] {
		import scala.language.postfixOps
 
		private[this] val expMovingAverage = 
				org.scalaml.filtering.movaverage.ExpMovingAverage[U](period)
		private val pfn = expMovingAverage |>
		
		/**
		 * Filter the time series xt with an exponential moving average
		 * @param x vector of input data
		 * @return A vector of Double values if computation is successful, None otherwise
		 */
		override def execute(x: Vector[U]): Try[DblVector] = pfn(x)
	}
	
    /**
		 * Wrapper for the Low-band filter based of the Discrete Fourier transform..
		 * @tparam  T type of single feature input data bounded (view) by Double
		 * @param g   Filtering function y = g(x, fC)used in the convolution
		 * @param fc  Frequency cutoff for this low pass filter.
		 * @constructor Create a wrapper low-pass filter using the discrete Fourier transform
		 * @throws IllegalArgumentException if the cut-off value is out of bounds
		 * @see Scala for machine learning Chapter 2 "Hello World!" modularization
		 * @see org.scalaml.filtering.DFTFir
		 */
	final class DFTFilter[U <: AnyVal](
			fc: Double)
			(g: (Double, Double) => Double)(implicit f: U => Double) extends Preprocessor[U]  {
	  import scala.language.postfixOps
	   
		private[this] val filter = org.scalaml.filtering.dft.DFTFilter[U](fc, 1e-5)(g)
		private[this] val pfn = filter |>
		
		override def execute(x: Vector[U]): Try[DblVector] = pfn(x).map(_.toVector)
  }
}



// ----------------------------------  EOF -------------------------------------
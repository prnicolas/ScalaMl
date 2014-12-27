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
package org.scalaml.workflow.module

import org.scalaml.core.Design.PipeOperator
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import org.apache.log4j.Logger
import org.scalaml.util.DisplayUtils


	/**
	 * <p>Clustering module used to instantiate a pre-processing/filtering component 
	 * in a workflow. The module can contain an arbitrary number of filtering algorithms. This
	 * class illustrates the injection dependency capabilities of Scala</p>
	 * 
	 * @author Patrick Nicolas
	 * @since January 23, 2014
	 * @note Scala for Machine Learning Chapter 2 Hello World! Designing a workflow
	 */
trait PreprocessingModule[T] {
	private val logger = Logger.getLogger("PreprocessingModule")
	type DblSeries = XTSeries[Double]
	implicit val convert = (t: T) => Double
  
		/**
		 * Clustering algorithm to be defined at run-time
		 */
	val preprocessor: Preprocessing[T]
		/**
		 * Base class for all pre-processing algorithms
		 */
	abstract class Preprocessing[T] {
		/**
		 * Pre-process a time series using this specific filtering algorithm
		 * @param xt time series
		 */
		def execute(xt: XTSeries[T]): Unit
	}


		/**
		 * <p>Wrapper for the exponential moving average defined in Chapter 3 for integration
		 * into a complex workflow.</p>
		 * @constructor Create an exponential moving average wrapper 
		 * @param period Period or size fo the time window in the moving average
		 * @throws IllegalArgumentException if period is non positive or alpha is out of range [0,1]
		 */
	final class ExpMovingAverage[T <% Double](period: Int)(implicit num: Numeric[T]) 
			extends Preprocessing[T] {
		import org.scalaml.filtering.ExpMovingAverage
		private[this] val expMovingAverage = ExpMovingAverage[T](period)
		
		/**
		 * Filter the time series xt with an exponential moving average
		 * @param xt time series
		 */
		override def execute(xt: XTSeries[T]): Unit = {
			try {
				val filtered = expMovingAverage |> xt
				DisplayUtils.show(filtered, logger)
			}
			catch {
				case e: MatchError => {
				  val errMsg = s"${e.getMessage} caused by ${e.getCause.toString}"
					DisplayUtils.error(s"PreprocessingModule.ExpMovingAverage $errMsg", logger)
				}
				case e: Throwable => DisplayUtils.error("PreprocessingModule.ExpMovingAverage", logger, e)
			}
		}
	}
  /**
		 * <p>Wrapper for the Low-band filter based of the Discrete Fourier transform..</p>
		 * @param g   Filtering function y = g(x, fC)used in the convolution
		 * @param fC  Frequency cutoff for this low pass filter.
		 * @constructor Create a wrapper low-pass filter using the discrete Fourier transform
		 * @throws IllegalArgumentException if the cut-off value is out of bounds
		 */
	final class DFTFir[T <% Double](
			g: (Double, Double) => Double,
			fc: Double) extends Preprocessing[T]  {
		private[this] val filter = new org.scalaml.filtering.DFTFir[T](g, fc)
		
		override def execute(xt: XTSeries[T]): Unit = {
			try {
				val filtered = filter |> xt
			}
			catch {
				case e: MatchError => {
					val errMsg = s"${e.getMessage} caused by ${e.getCause.toString}"
					DisplayUtils.error(s"PreprocessingModule.DFTFir $errMsg", logger)
				}
				case e: Throwable => DisplayUtils.error("PreprocessingModule.DFTFir", logger, e)
			}
		}
  }
}


// ----------------------------------  EOF -------------------------------------
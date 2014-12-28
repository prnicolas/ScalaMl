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
package org.scalaml.stats


import org.scalaml.core.Types.ScalaMl.{DblVector, XYTSeries}
import scala.util.{Try, Success, Failure}
import BiasVarianceEmulator._
import org.apache.log4j.Logger
import org.scalaml.util.DisplayUtils


		/**
		 * <p>Class to emulate the Bias-Variance decomposition using an emulation or synthetic 
		 * function to generate values. The purpose is to compute the bias and variance of a list 
		 * of single variable function y = f(x)</p>
		 * @constructor Create emulator function to compute the bias and variance of a list of single 
		 * variable function y = f(x).
		 * @param emul  Emulator for the bias-variance decomposition
		 * @param nValues Size of the dataset to use in the computation of Bias and Variance.
		 * @throws IllegalArgumentException if the number of values, nValues is out of range 
		 * @author Patrick Nicolas
		 * @since April 3, 2014
		 * @note Scala for Machine Learning Chapter 2 Hello World! / Assessing a model / 
		 * Bias-Variance decomposition
		 */
class BiasVarianceEmulator[T <% Double](emul: Double => Double, nValues: Int) {
	import BiasVarianceEmulator._

	check(nValues)
	private val logger = Logger.getLogger("BiasVarianceEmulator")

		/**
		 * <p>Compute the Bias and Variance for a list of model estimate extracted from 
		 * training data.</p>
		 * @param fEst list of function estimates 
		 * @return An option of array of tuple (Variance, Bias) for each function estimate, 
		 * if successful, None otherwise.
		 * @throws IllegalArgumentException if the list of function estimates are undefined
		 * @throws RuntimeException if a computation error occurs
		 */
	def fit(fEst: List[Double => Double]): Option[XYTSeries] = {
		require( !fEst.isEmpty, 
				"BiasVarianceEmulator.fit Cannot test the fitness of an undefined function")

		val rf = Range(0, fEst.size)
		Try {
				// Compute the mean value for the function estimate
			val meanFEst = Array.tabulate(nValues)(x => 
				rf.foldLeft(0.0)((s, n) => s + fEst(n)(x))/fEst.size)  
			
				// Collects the time series value for each function estimate
			val r = Range(0, nValues)
			fEst.map(estF => {
				r.foldLeft(0.0, 0.0)((s, x) => { 
					val diff = (estF(x) - meanFEst(x))/fEst.size 
					(s._1 + diff*diff, s._2 + Math.abs(estF(x) - emul(x) ))
				})
			}).toArray
		} 
		match {
			case Success(xySeries) => Some(xySeries)
			case Failure(e) => DisplayUtils.none("BiasVariance.fit ", logger, e)
		}
	}
}


		/**
		 * Object companion to BiasVarianceEmulator that defines its constructor and validate
		 * its parameters.
		 * @author Patrick Nicolas
		 * @since April 3, 2014
		 * @note Scala for Machine Learning Chapter 2 Hello World! / Assessing a model / 
		 * Bias-Variance decomposition
		 */
object BiasVarianceEmulator {
	private val NUMVALUES_LIMITS = (20, 20000)

		/**
		 * Default constructor for the BiasVarianceEmulator
		 * @param emul  Emulator for the bias-variance decomposition
		 * @param nValues Size of the dataset to use in the computation of Bias and Variance.
		 */
	def apply[T <% Double](emul: Double => Double, nValues: Int): BiasVarianceEmulator[T] 
		= new BiasVarianceEmulator[T](emul, nValues)
	          
	private def check(nValues: Int): Unit = {
		require( nValues > NUMVALUES_LIMITS._1 && nValues < NUMVALUES_LIMITS._2, 
				s"BiasVarianceEmulator.check Size of training sets $nValues is out of range")
	}
}

// -----------------------  EOF --------------------------------------
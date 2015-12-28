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
package org.scalaml.stats

  // Scala classes
import scala.util.{Try, Success, Failure}
	// 3rd party classes
import org.apache.log4j.Logger
	// Scala for Machine Learning classes and singletons
import org.scalaml.core.Types.ScalaMl.DblPair
import org.scalaml.util.LoggingUtils
// import LoggingUtils._, BiasVariance._


		/**
		 * Class to emulate the Bias-Variance decomposition using an emulation or synthetic 
		 * function to generate values. The purpose is to compute the bias and variance of a list 
		 * of single variable function '''y = f(x)'''
		 * @constructor Create emulator function to compute the bias and variance of a list of single
		 * variable function y = f(x).
		 * @param target  Emulator for the bias-variance decomposition
		 * @param nValues Size of the dataset to use in the computation of Bias and Variance.
		 * @throws IllegalArgumentException if the number of values, nValues is out of range 
		 * @author Patrick Nicolas
		 * @since 0.98.1 (April 3, 2014)
		 * @version 0.99.1
		 * @see Scala for Machine Learning Chapter 2 "Hello World!" / Assessing a model / 
		 * Bias-Variance decomposition
		 */
class BiasVariance(target: Double => Double, nValues: Int) {
	import BiasVariance._

	check(nValues)
	private val logger = Logger.getLogger("BiasVarianceEmulator")
	

		/**
		 * Compute the Bias and Variance for a list of model estimate extracted from 
		 * training data.
		 * @param models list of function estimates 
		 * @return An option of array of tuple (Variance, Bias) for each function estimate, 
		 * if successful, None otherwise.
		 * @throws IllegalArgumentException if the list of function estimates are undefined
		 * @throws RuntimeException if a computation error occurs
		 */
  @throws(classOf[IllegalArgumentException])
	def fit(models: List[Double => Double]): List[DblPair] = {
		require( models.nonEmpty,
				"BiasVarianceEmulator.fit Cannot test the fitness of an undefined function")

				// Compute the mean value for the different models		  
		val numModels = models.size
	//	val modelsMean = models.view.zipWithIndex.map{ case (f, n) => f(n)}.map(_ /numModels)
				
		  		// Collects the time series value for each function estimate
		models.map(accumulate(_, numModels))			
	}

	
	private def accumulate(f: Double => Double, numModels: Int): DblPair = {
	  val r = Range(0, nValues)
	  
	 	r./:(0.0, 0.0){ case ((s,t), x) =>
			val diff = (f(x) - target(x))/numModels
		  (s + diff*diff, t + Math.abs(f(x) -target(x)) )
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
object BiasVariance {
	private val NUMVALUES_LIMITS = (20, 20000)

		/**
		 * Default constructor for the BiasVarianceEmulator
		 * @param emul  Emulator for the bias-variance decomposition
		 * @param nValues Size of the dataset to use in the computation of Bias and Variance.
		 */
	def apply(emul: Double => Double, nValues: Int): BiasVariance
		= new BiasVariance(emul, nValues)
	          
	private def check(nValues: Int): Unit = {
		require( nValues > NUMVALUES_LIMITS._1 && nValues < NUMVALUES_LIMITS._2, 
				s"BiasVarianceEmulator.check Size of training sets $nValues is out of range")
	}
}

// -----------------------  EOF --------------------------------------
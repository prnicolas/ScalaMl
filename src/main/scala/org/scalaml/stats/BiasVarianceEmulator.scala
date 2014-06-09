/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.stats


import org.scalaml.core.Types.ScalaMl.{DblVector, XYTSeries}


	/**
	 * <p>Parameterized class to emulate the Bias-Variance computation for a target model
	 *  using a list of model estimates generated during training. It is assumed that the
	 *  training sets are identical in size.</p>
	 *  @param emul  target model that generates both training and validation data sets.
	 *  @param nValues number of data points in each training sets.
	 *  @exception IllegalArgumentException if the emulation model, emul, is undefined or the number of training samples is too small
	 *  @author Patrick Nicolas
	 *  @date April 3, 2014
	 *  @project Scala for Machine Learning
	 */

import BiasVarianceEmulator._
class BiasVarianceEmulator[T <% Double](val emul: Double => Double, val nValues: Int) {
    require( emul != null,  "Target or emulation model for Bias-Variance is undefined")
    require( nValues > minNValues,  "Size of training sets " + nValues + " for Bias Variance emulator is out of range")
    
    	/**
    	 * <p>Compute the Bias and Variance for a list of model estimate extracted from 
    	 * training data.</p>
    	 * @param fEst list of model estimators
    	 * @return An array of tuple (Variance, Bias) for each model estimators
    	 * @exception IllegalArgumentException if the model estimators are undefined
    	 * @exception RuntimeException if a computation error occurs
    	 */
	def fit(fEst: List[Double => Double]): Option[XYTSeries] = {
		require(fEst != null && fEst.size > 0, "Cannot test the fitness of an undefined function")

		val rf = Range(0, fEst.size)
		try {
			val meanFEst = Array.tabulate(nValues)( x => rf.foldLeft(0.0)((s, n) => s + fEst(n)(x))/fEst.size)  
			
			val r = Range(0, nValues)
			Some(fEst.map( estF => {
				r.foldLeft(0.0, 0.0)((s, x) => { 
					val diff = (estF(x) - meanFEst(x))/fEst.size 
					(s._1 + diff*diff, s._2 + Math.abs(estF(x) - emul(x) ))} )
			}).toArray)
		}
		catch {
			case e: RuntimeException => {
				Console.println("Failed to fit a list of models for Bias Validation emulator " + e.toString)
				None
			}
		}
	}
}


	/**
	 * Object companion to BiasVarianceEmulator that defines the constructor apply
	 * @author Patrick Nicolas
	 * @date April 3, 2014
	 * @project Scala for Machine Learning
	 */
object BiasVarianceEmulator {
	final val minNValues = 20
	def apply[T <% Double](emul: Double => Double, nValues: Int): BiasVarianceEmulator[T] = new BiasVarianceEmulator[T](emul, nValues)
}

// -----------------------  EOF --------------------------------------
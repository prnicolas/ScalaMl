/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.stats


import org.scalaml.core.Types.ScalaMl.{DblVector, XYTSeries}
import scala.util.{Try, Success, Failure}
import BiasVarianceEmulator._
import org.apache.log4j.Logger
import org.scalaml.util.Display



class BiasVarianceEmulator[T <% Double](val emul: Double => Double, val nValues: Int) {
    require( emul != null,  "Target or emulation model for Bias-Variance is undefined")
    require( nValues > minNValues,  "Size of training sets " + nValues + " for Bias Variance emulator is out of range")
    
    private val logger = Logger.getLogger("BiasVarianceEmulator")
    	/**
    	 * <p>Compute the Bias and Variance for a list of model estimate extracted from 
    	 * training data.</p>
    	 * @param fEst list of model estimators
    	 * @return An array of tuple (Variance, Bias) for each model estimators
    	 * @throws IllegalArgumentException if the model estimators are undefined
    	 * @throws RuntimeException if a computation error occurs
    	 */
	def fit(fEst: List[Double => Double]): Option[XYTSeries] = {
		require(fEst != null && fEst.size > 0, "Cannot test the fitness of an undefined function")

		val rf = Range(0, fEst.size)
		Try {
			val meanFEst = Array.tabulate(nValues)( x => rf.foldLeft(0.0)((s, n) => s + fEst(n)(x))/fEst.size)  
			
			val r = Range(0, nValues)
			fEst.map( estF => {
				r.foldLeft(0.0, 0.0)((s, x) => { 
					val diff = (estF(x) - meanFEst(x))/fEst.size 
					(s._1 + diff*diff, s._2 + Math.abs(estF(x) - emul(x) ))} )
			}).toArray
		} match {
			case Success(xySeries) => Some(xySeries)
			case Failure(e) => Display.error("BiasVariance.fit ", logger, e); None
		}
	}
}


	/**
	 * Object companion to BiasVarianceEmulator that defines the constructor apply
	 * @author Patrick Nicolas
	 * @since April 3, 2014
	 * @note Scala for Machine Learning
	 */
object BiasVarianceEmulator {
	final val minNValues = 20
	def apply[T <% Double](emul: Double => Double, nValues: Int): BiasVarianceEmulator[T] 
	          = new BiasVarianceEmulator[T](emul, nValues)
}

// -----------------------  EOF --------------------------------------
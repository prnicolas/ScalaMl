/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.supervised.hmm



import org.scalaml.util.Matrix
import org.scalaml.core.Types
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display

		/**
		 * <p>Implementation of the Beta or backward pass of the 
		 * HMM algorithm to compute the probability of a sequence of observations. The beta
		 * matrix is computed as part of the instantiation of the class.</p>
		 * @param lambdaB Lambda (pi, A, B) model for the HMM
		 * @param parms parameters used in any of the three canonical form of the HMM
		 * @param obsB: Array of observations as integer (categorical data)
		 * @throws IllegalArgumentException if lmbda, params and observations are undefined
		 * @author Patrick Nicolas
		 * @since March 14, 2014
		 */
protected class Beta(val lambdaB: HMMLambda, val obsB: Array[Int]) extends Pass(lambdaB, obsB) {
	private val logger = Logger.getLogger("Beta")
	
	val complete = {
		Try {
		    alphaBeta = Matrix[Double](lambda.dim._T, lambda.dim._N)	
		    alphaBeta += (lambda.d_1, 1.0)
		    normalize(lambda.d_1)
		    sumUp
		    true
		} match {
			case Success(t) => t
			case Failure(e) => Display.error("Failed beta computation ", logger, e); false
		}
	}
	
    private def sumUp: Unit = 
	   (lambda.dim._T-2 to 0 by -1).foreach( t =>{
	  	  updateBeta(t)
	  	  normalize(t) 
	   })
	  	
	private def updateBeta(t: Int): Unit =
  	   HMMDim.foreach(lambda.dim._N, i => { 
  	 	  alphaBeta += (t, i, lambda.beta(alphaBeta(t+1, i), i, labels(t+1)))	
  	   })
}


		/**
		 * Companion object for the Beta pass that defines the constructor apply
		 */
object Beta {
	def apply(lambda: HMMLambda,  _labels: Array[Int]): Beta = new Beta(lambda, _labels)
}




// --------------------------------  EOF -------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 * 
 * This code uses the iitb CRF library 
 * Copyright (c) <2004> <Sunita Sarawagi Indian Institute of Technology Bombay> All rights reserved.
 */
package org.scalaml.supervised.crf

import iitb.CRF.{CRF, CrfParams, DataSequence, DataIter, FeatureGenerator}
import iitb.Model.{FeatureGenImpl, CompleteModel}
import org.scalaml.core.XTSeries
import org.scalaml.workflow.data.DataSource
import org.scalaml.workflow.PipeOperator
import org.scalaml.supervised.Supervised
import java.io.IOException
import org.scalaml.core.Types.ScalaMl._
import CrfConfig._

	/**
	 * <p>Class that defines the basic stateuration of the CRF algorithm. The class generates a textual
	 * description of the stateuration of CRF used by iitb library </p>
	 * @param initLambda  initial values for the CRF weights/factors (lambdas)
	 * @param maxIters  Maximum number of iterations to be used for the training of CRF
	 * @param lambda L2-regularization penalty function 1/square(sigma) used in the log likelihood log p(Y|X)
	 * @param eps Convergence criteria used on the log likelihood  delta( log p(Y|X)to exit from the training iteration
	 * @param debug Optional debugging flag 
	 * 
	 * @author Patrick Nicolas
	 * @since April 3, 2014
	 */
protected class CrfConfig(val w0: Double, val maxIters: Int, val lambda: Double, val eps:Double, val debug: Int) {
    validate(w0, maxIters, lambda, eps)
    
    // textual description of the CRF stateuration
	val params = new StringBuilder().append("initValue ").append(String.valueOf(w0))
		           .append(" maxIters ").append(String.valueOf(maxIters)).append(" lambda ")
		              .append(String.valueOf(lambda)).append( " scale ")
		                 .append(" true" ).append(" epsForConvergence ").append(String.valueOf(eps) )
		                   .append(" debugLvl ").append(debug).toString

	private def validate(w0: Double, maxIters: Int, lambda: Double,  eps: Double): Unit = {
		require(w0 >= INIT_WEIGHTS_LIMITS._1 && w0 <= INIT_WEIGHTS_LIMITS._2, 
				        "Initialization of the CRF weights " + w0 + " is out of range")
		require( maxIters >= MAX_ITERS_LIMITS._1 && maxIters <= MAX_ITERS_LIMITS._2, 
				         "Maximum number of iterations for CRF training " + maxIters + " is out of range")
		require( lambda >= LAMBDA_LIMITS._1 && lambda <= LAMBDA_LIMITS._2, 
				         "The factor for the L2 penalty for CRF" + lambda + " is out of range")
		require( eps > EPS_LIMITS._1 && eps<= EPS_LIMITS._2, 
				          "The convergence criteria for the CRF training " + eps + " is out of range")
    }
}


		/**
		 * <p>Companion object for the stateuration of the conditional random field. The
		 * singleton is used to define constructors and boundaries for the class parameters..</p>
		 */
object CrfConfig {
	final val INIT_WEIGHTS_LIMITS = (0.1, 2.5)
	final val MAX_ITERS_LIMITS = (10, 250)
	final val LAMBDA_LIMITS = (1e-15, 1.5)
	final val EPS_LIMITS = (1e-5, 0.2)
	
	def apply(w0: Double, maxIters: Int, lambda: Double, eps:Double, debug: Int): CrfConfig = 
		            new CrfConfig(w0, maxIters, lambda, eps, debug)
	
    def apply(w0: Double, maxIters: Int, lambda: Double, eps:Double): CrfConfig = 
		            new CrfConfig(w0, maxIters, lambda, eps, 0)
}



// ---------------------------- EOF ------------------------------------------------------
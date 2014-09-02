/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
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

	/**
	 * <p>Class that defines the basic configuration of the CRF algorithm. The class generates a textual
	 * description of the configuration of CRF used by iitb library </p>
	 * @param initLambda  initial values for the CRF weights/factors (lambdas)
	 * @param maxIters  Maximum number of iterations to be used for the training of CRF
	 * @param lambda L2-regularization penalty function 1/square(sigma) used in the log likelihood log p(Y|X)
	 * @param eps Convergence criteria used on the log likelihood  delta( log p(Y|X)to exit from the training iteration
	 * @param debug Optional debugging flag 
	 * 
	 * @author Patrick Nicolas
	 * @since April 3, 2014
	 */
import CrfConfig._
class CrfConfig(val initW: Double, val maxIters: Int, val lambda: Double, val eps:Double, val debug: Int) {
    validate(initW, maxIters, lambda, eps)
    
    // textual description of the CRF configuration
	val params = new StringBuilder().append("initValue ").append(String.valueOf(initW))
		           .append(" maxIters ").append(String.valueOf(maxIters)).append(" lambda ")
		              .append(String.valueOf(lambda)).append( " scale ")
		                 .append(" true" ).append(" epsForConvergence ").append(String.valueOf(eps) )
		                   .append(" debugLvl ").append(debug).toString

	private def validate(initW: Double, maxIters: Int, lambda: Double,  eps: Double): Unit = {
		require( initW >= INIT_WEIGHTS_LIMITS._1 && initW <= INIT_WEIGHTS_LIMITS._2, 
				        "Initialization of the CRF weights " + initW + " is out of range")
		require( maxIters >= MAX_ITERS_LIMITS._1 && maxIters <= MAX_ITERS_LIMITS._2, 
				         "Maximum number of iterations for CRF training " + maxIters + " is out of range")
		require( lambda >= LAMBDA_LIMITS._1 && lambda <= LAMBDA_LIMITS._2, 
				         "The factor for the L2 penalty for CRF" + lambda + " is out of range")
		require( eps > EPS_LIMITS._1 && eps<= EPS_LIMITS._2, 
				          "The convergence criteria for the CRF training " + eps + " is out of range")
    }
}


		/**
		 * <p>Companion object for the configuration of the conditional random field. The
		 * singleton is used to define constructors and boundaries for the class parameters..</p>
		 */
object CrfConfig {
	final val INIT_WEIGHTS_LIMITS = (0.1, 2.5)
	final val MAX_ITERS_LIMITS = (10, 250)
	final val LAMBDA_LIMITS = (1e-15, 1.5)
	final val EPS_LIMITS = (1e-5, 0.2)
	
	def apply(initW: Double, maxIters: Int, lambda: Double, eps:Double, debug: Int): CrfConfig = 
		            new CrfConfig(initW, maxIters, lambda, eps, debug)
	
    def apply(initW: Double, maxIters: Int, lambda: Double, eps:Double): CrfConfig = 
		            new CrfConfig(initW, maxIters, lambda, eps, 0)
}



// ---------------------------- EOF ------------------------------------------------------
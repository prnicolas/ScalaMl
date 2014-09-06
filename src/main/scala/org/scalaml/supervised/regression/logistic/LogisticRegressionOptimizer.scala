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
package org.scalaml.supervised.regression.logistic

import org.apache.commons.math3.fitting.leastsquares._
import org.apache.commons.math3.fitting.leastsquares.LeastSquaresOptimizer.Optimum
import org.scalaml.core.Types.ScalaMl._



import LogisticRegressionOptimizer._
protected class LogisticRegressionOptimizer(val maxIters: Int, 
		                          val maxEvals: Int, 
		                          val eps: Double, 
		                          private val lsOptimizer: LeastSquaresOptimizer) {
	
  validate(maxIters, maxEvals, eps, lsOptimizer)

  def optimize(lsProblem: LeastSquaresProblem): Optimum = {
  	  require(lsProblem != null, "Cannot perform a Least Squares optimization with undefined Least square problem")
  	  lsOptimizer.optimize(lsProblem)
  }
  
  private def validate(maxIters: Int, maxEvals: Int, eps: Double, lsOptimizer: LeastSquaresOptimizer): Unit = {
	  require(maxIters >=  NUM_ITERS_LIMITS._1 && maxIters <= NUM_ITERS_LIMITS._2, 
				   "Maximum number of iterations " + maxIters + " is out of range")
	  require(maxEvals >=  NUM_EVALS_LIMITS._1 && maxEvals <= NUM_EVALS_LIMITS._2, 
				   "Maximum number of evaluations " + maxEvals + " is out of range")			   
	  require(maxIters < maxEvals, 
				   "Maximum number of iterations " + maxIters + " exceeds maximum number of evaluations" + maxEvals)
	  require( eps >= EPS_LIMITS._1 && eps <= EPS_LIMITS._2, "eps for the optimization of the logistic regression " + eps + " is out of range")
	  require(lsOptimizer != null, "Least squares optimizer for the logistic regression is undefined")
  }
}


object LogisticRegressionOptimizer {
	final val EPS_LIMITS =  (1e-32,  1.0)
	final val NUM_ITERS_LIMITS = (10, 1000)
	final val NUM_EVALS_LIMITS = (100, 10000)
	
	final val DEFAULT_NUM_ITERS = 50
	final val DEFAULT_NUM_EVALS = 100
	final val DEFAULT_EPS = 1e-2
	
	def apply(maxIters: Int, maxEvals: Int, eps: Double, lsOptimizer: LeastSquaresOptimizer): LogisticRegressionOptimizer = new LogisticRegressionOptimizer(maxIters, maxEvals, eps, lsOptimizer)
    def apply(eps: Double, lsOptimizer: LeastSquaresOptimizer): LogisticRegressionOptimizer = new LogisticRegressionOptimizer(DEFAULT_NUM_ITERS, DEFAULT_NUM_EVALS, eps, lsOptimizer)
	def apply(lsOptimizer: LeastSquaresOptimizer): LogisticRegressionOptimizer = new LogisticRegressionOptimizer(DEFAULT_NUM_ITERS, DEFAULT_NUM_EVALS, DEFAULT_EPS, lsOptimizer)
}


//---------------------------------  EOF ---------------------------------------
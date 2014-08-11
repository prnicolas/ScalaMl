/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.supervised.svm

import libsvm._
import org.scalaml.supervised.Config


	/**
	 * <p>Generic configuration manager for any category of SVM algorithm. The configuration of a SVM has
	 * three elements:<br>
	 * - Type and parameters of the formulation of the SVM algorithm<br>
	 * - Type and parameter(s) of the Kernel function used for non-linear problems<br>
	 * - Execution (training) configuration parameters.</p>
	 * @param formulation formulation of the SVM problem
	 * @param kernel kernel function used for non-separable training sets
	 * @param svmParms parameters for the training of the SVM model
	 * 
	 * @author Patrick Nicolas
	 * @date April 30, 2014
	 * @project Scala for Machine Learning
	 */
final protected class SVMConfig(val formula: SVMFormulation, val kernel: SVMKernel, val exec: SVMExecution) extends Config {
	validate(formula, kernel, exec)
	
    val  param = new svm_parameter
    formula.config(param)
    kernel.config(param)
    exec.config(param)
    
    override def toString: String = {
       val buf = new StringBuilder("\nSVM Formulation: ")
       buf.append(param.svm_type)
    	     .append("\ngamma: ")
    	       .append(param.gamma)
    	         .append("\nProbability: ")
    	           .append(param.probability)
    	             .append("\nWeights: ")
    	             
       if( param.weight != null) {
         for( w <- param.weight)
      	   buf.append(w).append(",")
       }
       else 
      	  buf.append(" -no weight")
      buf.toString
    }
    
    @inline def eps: Double = exec.eps
    
    @inline
    def isCrossValidation: Boolean = exec.nFolds > 0
    
    private def validate(formulation: SVMFormulation, kernel: SVMKernel, svmParams: SVMExecution): Unit =  {
		require(formulation != null, "Formulation in the configuration of SVM is undefined")
		require(kernel != null, "Kernel function in the configuration of SVM is undefined")
		require(svmParams != null, "The training execution parameters in the configuration of SVM is undefined")	
	}
}


		/**
		 * <p>Companion object for SVM configuration manager used for defining the constructors of SVMConfig class.</p>
		 */
object SVMConfig {
   final val DEFAULT_CACHE = 25000
   final val DEFAULT_EPS = 1e-15
   
   def apply(svmType: SVMFormulation, kernel: SVMKernel, svmParams: SVMExecution): SVMConfig = new SVMConfig(svmType, kernel, svmParams)
   def apply(svmType: SVMFormulation, kernel: SVMKernel): SVMConfig = new SVMConfig(svmType, kernel, new SVMExecution(DEFAULT_CACHE, DEFAULT_EPS, -1))
}

// --------------------------- EOF ------------------------------------------
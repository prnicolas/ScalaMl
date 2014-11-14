/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95d
 */
package org.scalaml.supervised.svm

import libsvm._
import org.scalaml.core.design.Config
import org.scalaml.supervised.svm.kernel.SVMKernel
import org.scalaml.supervised.svm.formulation.SVMFormulation


		/**
		 * <p>Generic configuration item for support vector machine.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since April 28, 2014
		 * @note Scala for Machine Learning Chapter 8 Kernel models and support vector machines
		 */
trait SVMConfigItem {
		/**
		 * <p>Update the LIBSVM configuration parameter.</p>
		 * @param param LIBSVM parameter to update.
		 */
	 def update(param: svm_parameter): Unit
}



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
	 * @since April 30, 2014
	 * @note Scala for Machine Learning
	 */
final protected class SVMConfig(formulation: SVMFormulation, kernel: SVMKernel, exec: SVMExecution) extends Config {
	import SVMConfig._
	
	check(formulation, kernel, exec)
	val persists = "config/svm"
	  
    val  param = new svm_parameter
    formulation.update(param)
    kernel.update(param)
    exec.update(param)
    
    override def toString: String = {
       val buf = new StringBuilder("\nSVM Formulation: ${param.svm_type}\ngamma: ${param.gamma}\nProbability: ${param.probability}\nWeights: ")
    	             
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
    
    @inline
    def nFolds: Int = exec.nFolds

}



		/**
		 * <p>Companion object for SVM configuration manager used for defining the constructors of SVMConfig class.</p>
		 */
object SVMConfig {
   final val DEFAULT_CACHE = 25000
   final val DEFAULT_EPS = 1e-15
   
   def apply(svmType: SVMFormulation, kernel: SVMKernel, svmParams: SVMExecution): SVMConfig = new SVMConfig(svmType, kernel, svmParams)
   def apply(svmType: SVMFormulation, kernel: SVMKernel): SVMConfig = new SVMConfig(svmType, kernel, new SVMExecution(DEFAULT_CACHE, DEFAULT_EPS, -1))
   
       
    private def check(formulation: SVMFormulation, kernel: SVMKernel, svmParams: SVMExecution): Unit =  {
		require(formulation != null, "Formulation in the stateuration of SVM is undefined")
		require(kernel != null, "Kernel function in the stateuration of SVM is undefined")
		require(svmParams != null, "The training execution parameters in the stateuration of SVM is undefined")	
	}
}



// --------------------------- EOF ------------------------------------------
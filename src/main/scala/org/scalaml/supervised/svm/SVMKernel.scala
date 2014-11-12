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


trait SVMConfigItem {
	 def update(param: svm_parameter): Unit
}


		/**
		 * <p>Generic trait for Kernel functions used for Support Vector Machine.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since April 30, 2014
		 * @note Scala for Machine Learning Chapter 8 
		 */
sealed trait SVMKernel extends SVMConfigItem {
   def update(param: svm_parameter): Unit
}


object SVMKernel {
	final val GAMMA_LIMITS = (1e-17, 1e+5)
	final val DEGREE_LIMITS = (1, 10000)
}

import SVMKernel._

	    /**
		 * <p>Definition of the Linear kernel.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since April 30, 2014
		 * @note Scala for Machine Learning
		 */
object LinearKernel extends SVMKernel {
	 /**
  	 * <p>Initialize the LIBSVM type and parameter of the Kernel function.</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @throws IllegalArgumentException if param is undefined.
  	 */
   override def update(param: svm_parameter): Unit = {
   	  require(param != null, "Cannot stateure undefined LIBSVM svm parameter class with kernel")
      param.kernel_type = svm_parameter.LINEAR
   }

    override def toString: String = "\nLinear kernel"
}


	    /**
		 * <p>Definition of the Radial Basis Kernel function. The radial basis function is implemented
		 * as a Gaussian function.</p>
		 * @param gamma gamma or scaling parameter for the RBF kernel
		 * @throws IllegalArgumentException if gamma is negative or null
		 * 
		 * @author Patrick Nicolas
		 * @since April 30, 2014
		 * @note Scala for Machine Learning
		 */
case class RbfKernel(gamma: Double) extends SVMKernel {
	require(gamma >= GAMMA_LIMITS._1 && gamma <= GAMMA_LIMITS._2, s"Gamma for the RBF kernel $gamma is out of range")
    
	/**
  	 * <p>Initialize the LIBSVM type and parameter of the Kernel function.</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @throws IllegalArgumentException if param is undefined.
  	 */
    override def update(param: svm_parameter): Unit = {
      require(param != null, "Cannot stateure undefined LIBSVM svm parameter class with kernel") 
    	
      param.kernel_type = svm_parameter.RBF
      param.gamma = gamma
    }
    
    override def toString: String = "\nRBF " + String.valueOf(gamma)
}


	    /**
		 * <p>Definition of the Sigmoid Kernel function.</p>
		 * @param gamma gamma or scaling parameter for the Sigmoid kernel
		 * @throws IllegalArgumentException if gamma is negative or null
		 * 
		 * @author Patrick Nicolas
		 * @since April 30, 2014
		 * @note Scala for Machine Learning
		 */
case class SigmoidKernel(gamma: Double) extends SVMKernel {
	require(gamma >= GAMMA_LIMITS._1 && gamma <= GAMMA_LIMITS._2, s"Gamma for the Sigmoid kernel $gamma is out of range")
		
	/**
  	 * <p>Initialize the LIBSVM type and parameter of the Kernel function.</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @throws IllegalArgumentException if param is undefined.
  	 */
	override def update(param: svm_parameter): Unit = {
	  require(param != null, "Cannot stateure undefined LIBSVM svm parameter class with kernel")
			  
      param.kernel_type = svm_parameter.SIGMOID
      param.gamma = gamma
    }
    
    override def toString: String = "\nSIGMOID " + String.valueOf(gamma)
}


	    /**
		 * <p>Definition of the polynomial Kernel function.</p>
		 * @param gamma gamma or scaling parameter for the polynomial kernel
		 * @param coef0 b coefficient (order 0) for the polynomial kernel
		 * @param degree degree or power of the polynomial kernel
		 * @throws IllegalArgumentException if gamma is negative or null or if degree < 1
		 * 
		 * @author Patrick Nicolas
		 * @since April 30, 2014
		 * @note Scala for Machine Learning
		 */
case class PolynomialKernel(gamma: Double, coef0: Double, degree: Int) extends SVMKernel {
	require(gamma >= GAMMA_LIMITS._1 && gamma <= GAMMA_LIMITS._2, s"Gamma for the polynomial kernel $gamma is out of range")
	require(degree >= DEGREE_LIMITS._1 && degree <= DEGREE_LIMITS._2, s"The degree of the polynomial kernel $degree is out of range")
	
    /**
  	 * <p>Initialize the LIBSVM type and parameter of the Kernel function.</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @throws IllegalArgumentException if param is undefined.
  	 */
   override def update(param: svm_parameter): Unit = {
  	  require(param != null, "Cannot stateure undefined LIBSVM svm parameter class with kernel")
  	   	 
      param.kernel_type = svm_parameter.POLY
      param.gamma = gamma
      param.coef0 = coef0
      param.degree = degree
    }
    
    override def toString: String = "\nPOLYNOMIAL " + String.valueOf(gamma) + ", " + String.valueOf(coef0) + ", "+ String.valueOf(degree)
}


// --------------------------- EOF ------------------------------------------
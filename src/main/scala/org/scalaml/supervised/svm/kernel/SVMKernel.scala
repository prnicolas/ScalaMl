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
package org.scalaml.supervised.svm.kernel

import libsvm._
import org.scalaml.supervised.svm.SVMConfigItem


		/**
		 * Generic trait for Kernel functions used for Support Vector Machine.
		 * @author Patrick Nicolas
		 * @since 0.98 (April 30, 2014)
		 * @see Scala for Machine Learning Chapter 8 Kernel models and support vector machines / 
		 * Kernel functions
		 */
sealed trait SVMKernel extends SVMConfigItem {
		/**
		 * Initialize the LIBSVM type and parameter of the Kernel function.
		 * @param param svm_parameter LIBSVM instance to initialize
		 */
	def update(param: svm_parameter): Unit
}

		/**
		 * Companion object for the SVMKernel, used to defined the bound values
		 * for the Gamma and Degree coefficients for concrete kernel function
		 */
object SVMKernel {
	final val GAMMA_LIMITS = (1e-17, 1e+5)
	final val DEGREE_LIMITS = (1, 10000)
	
	
}

import SVMKernel._

	/**
		 * Definition of the Linear kernel.
		 * 
		 * @author Patrick Nicolas
		 * @since April 30, 2014
		 * @note Scala for Machine Learning Chapter 8 Kernel models and support vector machines / 
		 * Kernel functions
		 */
object LinearKernel extends SVMKernel {
		/**
		 * Initialize the LIBSVM type and parameter of the Kernel function.
		 * @param param svm_parameter LIBSVM instance to initialize
		 * @throws IllegalArgumentException if param is undefined.
		 */
	override def update(param: svm_parameter): Unit = {
		require(param != null, "LinearKernel.update LIBSVM svm parameter is undefined")
		param.kernel_type = svm_parameter.LINEAR
	}
		/**
		 * Textual description of the Linear kernel
		 */
	override def toString: String = "Linear kernel"
}

		/**
		 * Definition of the Radial Basis Kernel function. The radial basis function is implemented
		 * as a Gaussian function.
		 * @constructor create a Radial Basis function kernel,
		 * @throws IllegalArgumentException if gamma is negative or null
		 * @param gamma  Gamma or scaling parameter for the RBF kernel.
		 * @author Patrick Nicolas
		 * @since April 30, 2014
		 * @note Scala for Machine Learning Chapter 8 Kernel models and support vector machines / 
		 * Kernel functions
		 */
private[scalaml] final class RbfKernel(gamma: Double) extends SVMKernel {
	require(gamma >= GAMMA_LIMITS._1 && gamma <= GAMMA_LIMITS._2, 
			s"RbfKernel Gamma for the RBF kernel $gamma is out of range")
    
		/**
		 * Initialize the LIBSVM type and parameter of the Kernel function.
		 * @param param svm_parameter LIBSVM instance to initialize
		 * @throws IllegalArgumentException if param is undefined.
		 */
	override def update(param: svm_parameter): Unit = {
		require(param != null, "RbfKernel.update LIBSVM svm parameter is undefined") 
    	
		param.kernel_type = svm_parameter.RBF
		param.gamma = gamma
	}
		/**
		 * Textual description of the Linear kernel
		 */
	override def toString: String = s"Radial-basis function with gamma = $gamma"
}


		/**
		 * Definition of the Sigmoid Kernel function.
		 * @throws IllegalArgumentException if gamma is negative or null
		 * @param gamma  Gamma or scaling parameter for the Sigmoid kernel.
		 * @author Patrick Nicolas
		 * @since April 30, 2014
		 * @note Scala for Machine Learning  Chapter 8 Kernel models and support vector machines / 
		 * Kernel functions
		 */
private[scalaml] final class SigmoidKernel(gamma: Double) extends SVMKernel {
	require(gamma >= GAMMA_LIMITS._1 && gamma <= GAMMA_LIMITS._2, 
			s"SigmoidKernel Gamma for the Sigmoid kernel $gamma is out of range")
		
		/**
		 * Initialize the LIBSVM type and parameter of the Kernel function.
		 * @param param svm_pa rameter LIBSVM instance to initialize
		 * @throws IllegalArgumentException if param is undefined.
		 */
	override def update(param: svm_parameter): Unit = {
		require(param != null, "SigmoidKernel.update LIBSVM svm parameter is undefined")
			  
		param.kernel_type = svm_parameter.SIGMOID
		param.gamma = gamma
	}
		/**
		 * Textual description of the Linear kernel
		 */
	override def toString: String = s"Sigmoidal function with gamma = $gamma"
}


		/**
		 * Definition of the polynomial Kernel function.
		 * @constructor Create a polynomial kernel functionj with a given gamma, intercept coef and 
		 * degree
		 * @throws IllegalArgumentException if gamma is negative or null or if degree < 1
		 * @param gamma  Gamma or scaling parameter for the Polynomial kernel.
		 * @param coef0 Intercept or coefficient of order 0 for the polynomial kernel
		 * @param degree  Degree or power of the polynomial kernel.
		 * @author Patrick Nicolas
		 * @since April 30, 2014
		 * @note Scala for Machine Learning Chapter 8 Kernel models and support vector machines / 
		 * Kernel functions
		 */
private[scalaml] final class PolynomialKernel(gamma: Double, coef0: Double, degree: Int) extends SVMKernel {
	require(gamma >= GAMMA_LIMITS._1 && gamma <= GAMMA_LIMITS._2, 
			s"PolynomialKernel Gamma for the polynomial kernel $gamma is out of range")
	require(degree >= DEGREE_LIMITS._1 && degree <= DEGREE_LIMITS._2, 
			s"PolynomialKernel The degree of the polynomial kernel $degree is out of range")
	
		/**
		 * Initialize the LIBSVM type and parameter of the Kernel function.
		 * @param param svm_parameter LIBSVM instance to initialize
		 * @throws IllegalArgumentException if param is undefined.
		 */
	override def update(param: svm_parameter): Unit = {
		require(param != null, "PolynomialKernel.update LIBSVM svm parameter is undefined")
  	   	 
		param.kernel_type = svm_parameter.POLY
		param.gamma = gamma
		param.coef0 = coef0
		param.degree = degree
	}
 		/**
		 * Textual description of the Linear kernel
		 */
	override def toString: String = 
			s"Polynomial function, gamma: $gamma, coef: $coef0, degree: $degree"
}


// --------------------------- EOF ------------------------------------------
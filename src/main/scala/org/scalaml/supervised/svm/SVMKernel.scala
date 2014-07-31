/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.supervised.svm

import libsvm._


trait SVMConfigItem {
	 def config(param: svm_parameter): Unit
}


		/**
		 * <p>Generic trait for Kernel functions used for Support Vector Machine.</p>
		 * 
		 * @author Patrick Nicolas
		 * @date April 30, 2014
		 * @project Scala for Machine Learning
		 */
sealed trait SVMKernel extends SVMConfigItem {
   def config(param: svm_parameter): Unit
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
		 * @date April 30, 2014
		 * @project Scala for Machine Learning
		 */
object LinearKernel extends SVMKernel {
	 /**
  	 * <p>Initialize the LIBSVM type and parameter of the Kernel function.</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @exception IllegalArgumentException if param is undefined.
  	 */
   override def config(param: svm_parameter): Unit = {
   	  require(param != null, "Cannot configure undefined LIBSVM svm parameter class with kernel")
      param.kernel_type = svm_parameter.LINEAR
   }

    override def toString: String = "\nLinear kernel"
}


	    /**
		 * <p>Definition of the Radial Basis Kernel function. The radial basis function is implemented
		 * as a Gaussian function.</p>
		 * @param gamma gamma or scaling parameter for the RBF kernel
		 * @exception IllegalArgumentException if gamma is negative or null
		 * 
		 * @author Patrick Nicolas
		 * @date April 30, 2014
		 * @project Scala for Machine Learning
		 */
case class RbfKernel(val gamma: Double) extends SVMKernel {
	require(gamma >= GAMMA_LIMITS._1 && gamma <= GAMMA_LIMITS._2, "Gamma for the RBF kernel " + gamma + " is out of range")
    
	/**
  	 * <p>Initialize the LIBSVM type and parameter of the Kernel function.</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @exception IllegalArgumentException if param is undefined.
  	 */
    override def config(param: svm_parameter): Unit = {
      require(param != null, "Cannot configure undefined LIBSVM svm parameter class with kernel") 
    	
      param.kernel_type = svm_parameter.RBF
      param.gamma = gamma
    }
    
    override def toString: String = "\nRBF " + String.valueOf(gamma)
}


	    /**
		 * <p>Definition of the Sigmoid Kernel function.</p>
		 * @param gamma gamma or scaling parameter for the Sigmoid kernel
		 * @exception IllegalArgumentException if gamma is negative or null
		 * 
		 * @author Patrick Nicolas
		 * @date April 30, 2014
		 * @project Scala for Machine Learning
		 */
case class SigmoidKernel(val gamma: Double) extends SVMKernel {
	require(gamma >= GAMMA_LIMITS._1 && gamma <= GAMMA_LIMITS._2, "Gamma for the Sigmoid kernel " + gamma + " is out of range")
		
	/**
  	 * <p>Initialize the LIBSVM type and parameter of the Kernel function.</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @exception IllegalArgumentException if param is undefined.
  	 */
	override def config(param: svm_parameter): Unit = {
	  require(param != null, "Cannot configure undefined LIBSVM svm parameter class with kernel")
			  
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
		 * @exception IllegalArgumentException if gamma is negative or null or if degree < 1
		 * 
		 * @author Patrick Nicolas
		 * @date April 30, 2014
		 * @project Scala for Machine Learning
		 */
case class PolynomialKernel(val gamma: Double, val coef0: Double, val degree: Int) extends SVMKernel {
	require(gamma >= GAMMA_LIMITS._1 && gamma <= GAMMA_LIMITS._2, "Gamma for the polynomial kernel" + gamma + " is out of range")
	require(degree >= DEGREE_LIMITS._1 && degree <= DEGREE_LIMITS._2, "The degree of the polynomial kernel " + degree + " is out of range")
	
    /**
  	 * <p>Initialize the LIBSVM type and parameter of the Kernel function.</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @exception IllegalArgumentException if param is undefined.
  	 */
   override def config(param: svm_parameter): Unit = {
  	  require(param != null, "Cannot configure undefined LIBSVM svm parameter class with kernel")
  	   	 
      param.kernel_type = svm_parameter.POLY
      param.gamma = gamma
      param.coef0 = coef0
      param.degree = degree
    }
    
    override def toString: String = "\nPOLYNOMIAL " + String.valueOf(gamma) + ", " + String.valueOf(coef0) + ", "+ String.valueOf(degree)
}


// --------------------------- EOF ------------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.supervised.svm

import libsvm._


	/**
	 * <p>Trait for type or formulation of Support Vector Machine algorithms.</p>
	 * @author Patrick Nicolas
	 * @since April 29, 2014
	 * @note Scala for Machine Learning
	 */
sealed trait SVMFormulation extends SVMConfigItem {
  def config(param: svm_parameter): Unit 
}

object SVMFormulation {
	final val NU_LIMITS = (0.0, 1.0)
	final val C_LIMITS = (0.0, 100.0)
	final val EPSILON_LIMITS = (0.0, 100.0)
}

import SVMFormulation._


	/**
	 * <p>Class to initialize the type of SVM to a C formulated support vector classifier.</p>
	 * @param c C-penalty or inverse regularization factor
	 * @throws IllegalArgumentException if c is negative
	 * 
	 * @author Patrick Nicolas
	 * @since April 29, 2014
	 * @note Scala for Machine Learning
	 */
case class CSVCFormulation(val c: Double) extends SVMFormulation {
  require(c >= C_LIMITS._1 && c <= C_LIMITS._2, "C penalty factor " + c + " is out of range")
  
  	/**
  	 * <p>Initialize the LIBSVM parameters configuration</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @throws IllegalArgumentException if param is undefined.
  	 */
  override def config(param: svm_parameter): Unit = {
  	 require(param != null, "Cannot configure undefined LIBSVM svm parameter class")
  	 
     param.svm_type = svm_parameter.C_SVC
     param.C = c
  }
  
  override def toString: String = "\nC-SVC: C=" + String.valueOf(c)
}

	/**
	 * <p>Class to initialize the type of SVM to a Nu formulated support vector classifier.</p>
	 * @param nu normalized penalty factor
	 * @param rho rho factor for the Nu formulation of the Nu support vector classifiers
	 * @throws IllegalArgumentException if nu is out of range [0,1]
	 * 
	 * @author Patrick Nicolas
	 * @since April 29, 2014
	 * @note Scala for Machine Learning
	 */
case class NuSVCFormulation(val nu: Double, val rho: Double) extends SVMFormulation {
  require(nu >= NU_LIMITS._1 && nu <= NU_LIMITS._2, "nu penalty factor " + nu + " is out of range")  
  
    /**
  	 * <p>Initialize the LIBSVM parameters configuration</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @throws IllegalArgumentException if param is undefined.
  	 */
  override def config(param: svm_parameter): Unit = {
  	 require(param != null, "Cannot configure undefined LIBSVM svm parameter class")
  	 	 
     param.svm_type = svm_parameter.NU_SVC
     param.nu = nu
     param.p =rho
  }
  
  override def toString: String = new StringBuilder("\nNu-SVC: nu= ").append(nu).append(" rho= ").append(rho).toString
}


	/**
	 * <p>Class to initialize the type of SVM to a one class support vector classifier for outliers.</p>
	 * @param nu normalized penalty factor
	 * @throws IllegalArgumentException if nu is out of range [0,1]
	 * 
	 * @author Patrick Nicolas
	 * @since April 29, 2014
	 * @note Scala for Machine Learning
	 */
case class OneSVCFormulation(val nu: Double) extends SVMFormulation {
  require(nu >= NU_LIMITS._1 && nu <= NU_LIMITS._2, "nu penalty factor " + nu + " is out of range")  
  
    /**
  	 * <p>Initialize the LIBSVM parameters configuration</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @throws IllegalArgumentException if param is undefined.
  	 */
  override def config(param: svm_parameter): Unit = {
	 require(param != null, "Cannot configure undefined LIBSVM svm parameter class")
  	 
     param.svm_type = svm_parameter.ONE_CLASS
     param.nu = nu
  }
  
  override def toString: String = new StringBuilder("\nOne-class SVC: nu= ").append(nu).toString
}


	/**
	 * <p>Class to initialize the type of SVM to the epsilon formulation of the support vector regressions.</p>
	 * @param c C-penalty (regularization) factor
	 * @param epsilon epsilon factor for the support vector regression
	 * @throws IllegalArgumentException if nu is out of range [0,1]
	 * 
	 * @author Patrick Nicolas
	 * @since April 29, 2014
	 * @note Scala for Machine Learning
	 */
case class SVRFormulation(val c: Double, val epsilon: Double) extends SVMFormulation { 
   require(c >= C_LIMITS._1 && c <= C_LIMITS._2, "C penalty factor " + c + " is out of range")
   require(epsilon >= EPSILON_LIMITS._1 && epsilon <= EPSILON_LIMITS._2, "Epsilon factor " + epsilon + " is out of range") 
   
     /**
  	 * <p>Initialize the LIBSVM parameters configuration</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @throws IllegalArgumentException if param is undefined.
  	 */
  override def config(param: svm_parameter): Unit = {
  	 require(param != null, "Cannot configure undefined LIBSVM svm parameter class")
  	 
     param.svm_type = svm_parameter.EPSILON_SVR
     param.C = c
     param.p = epsilon
  }
  
  override def toString: String = new StringBuilder("\nEpsilon-SVR: C= ").append(c).append(" epsilon=").append(epsilon).toString
}


// --------------------------- EOF ------------------------------------------
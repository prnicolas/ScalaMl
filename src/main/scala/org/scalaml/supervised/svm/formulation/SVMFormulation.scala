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
package org.scalaml.supervised.svm.formulation

import libsvm._
import org.scalaml.supervised.svm.SVMConfigItem

	/**
	 * <p>Trait for type or formulation of Support Vector Machine algorithms.</p>
	 * @author Patrick Nicolas
	 * @since April 29, 2014
	 * @note Scala for Machine Learning
	 */
sealed trait SVMFormulation extends SVMConfigItem {
  def update(param: svm_parameter): Unit 
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
class CSVCFormulation(c: Double) extends SVMFormulation {
  require(c >= C_LIMITS._1 && c <= C_LIMITS._2, "C penalty factor " + c + " is out of range")
  
  	/**
  	 * <p>Initialize the LIBSVM parameters stateuration</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @throws IllegalArgumentException if param is undefined.
  	 */
  override def update(param: svm_parameter): Unit = {
  	 require(param != null, "CSVCFormulation: Undefined LIBSVM svm parameter class")
  	 
     param.svm_type = svm_parameter.C_SVC
     param.C = c
  }
  
  override def toString: String = s"\nC-SVC: C=${String.valueOf(c)}"
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
class NuSVCFormulation(nu: Double, rho: Double) extends SVMFormulation {
  require(nu >= NU_LIMITS._1 && nu <= NU_LIMITS._2, s"NuSVCFormulation: Nu penalty factor $nu is out of range")  
  
    /**
  	 * <p>Initialize the LIBSVM parameters stateuration</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @throws IllegalArgumentException if param is undefined.
  	 */
  override def update(param: svm_parameter): Unit = {
  	 require(param != null, "NuSVCFormulation: Undefined LIBSVM svm parameter class")
  	 	 
     param.svm_type = svm_parameter.NU_SVC
     param.nu = nu
     param.p =rho
  }
  
  override def toString: String = s"\nNu-SVC: nu= $nu, rho= $rho"
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
class OneSVCFormulation(nu: Double) extends SVMFormulation {
  require(nu >= NU_LIMITS._1 && nu <= NU_LIMITS._2, s"nu penalty factor $nu is out of range")  
  
    /**
  	 * <p>Initialize the LIBSVM parameters stateuration</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @throws IllegalArgumentException if param is undefined.
  	 */
  override def update(param: svm_parameter): Unit = {
	 require(param != null, "OneSVCFormulation Cannot Undefined LIBSVM svm parameter class")
  	 
     param.svm_type = svm_parameter.ONE_CLASS
     param.nu = nu
  }
  
  override def toString: String = s"\nOne-class SVC: nu= $nu"
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
class SVRFormulation(c: Double, epsilon: Double) extends SVMFormulation { 
   require(c >= C_LIMITS._1 && c <= C_LIMITS._2, s"C penalty factor $c is out of range")
   require(epsilon >= EPSILON_LIMITS._1 && epsilon <= EPSILON_LIMITS._2, s"Epsilon factor $epsilon is out of range") 
   
     /**
  	 * <p>Initialize the LIBSVM parameters stateuration</p>
  	 * @param param svm_parameter LIBSVM instance to initialize
  	 * @throws IllegalArgumentException if param is undefined.
  	 */
  override def update(param: svm_parameter): Unit = {
  	 require(param != null, "SVRFormulation Undefined LIBSVM svm parameter class")
  	 
     param.svm_type = svm_parameter.EPSILON_SVR
     param.C = c
     param.p = epsilon
  }
  
  override def toString: String = s"\nEpsilon-SVR: C = $c, epsilon= $epsilon"
}


// --------------------------- EOF ------------------------------------------
/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.supervised.svm.formulation

import libsvm._
	// ScalaMl classes
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

		/**
		 * Companion object for the SVM Formulation trait
		 */
object SVMFormulation {
	val NU_LIMITS = (0.0, 1.0)
	val C_LIMITS = (0.0, 100.0)
	val EPSILON_LIMITS = (0.0, 100.0)
}

import SVMFormulation._


		/**
		 * <p>Class to initialize the type of SVM to a C formulated support vector classifier.</p>
		 * @constructor Create a C-SVC formulation
		 * @param c  C-penalty or inverse regularization factor.
		 * @throws IllegalArgumentException if c is negative
		 * @author Patrick Nicolas
		 * @since April 29, 2014
		 * @note Scala for Machine Learning Chapter 8 Kernel Models and Support Vector Machines
		 */
final class CSVCFormulation(c: Double) extends SVMFormulation {
	require(c >= C_LIMITS._1 && c <= C_LIMITS._2, 
			s"CSVCFormulation C penalty factor $c is out of range")
  
		/**
		 * <p>Initialize the LIBSVM parameters configuration</p>
		 * @param param svm_parameter LIBSVM instance to initialize
		 * @throws IllegalArgumentException if param is undefined.
		 */
	override def update(param: svm_parameter): Unit = {
		require(param != null, "CSVCFormulation.update Undefined LIBSVM svm parameter class")
  	 
		param.svm_type = svm_parameter.C_SVC
		param.C = c
	}
	/**
		 * Textual description of the C-(1/lambda) formulation for the Support Vector Regression.
		 */
	override def toString: String = s"C-SVC with C = ${String.valueOf(c)}"
}

		/**
		 * <p>Class to initialize the type of SVM to a Nu formulated support vector classifier.</p>
		 * @constructor Create a Nu-SVC formulation
		 * @throws IllegalArgumentException if nu is out of range [0,1]
		 * @param nu normalized penalty factor
		 * @param rho rho factor for the Nu formulation of the Nu support vector classifiers
		 * @author Patrick Nicolas
		 * @since April 29, 2014
		 * @note Scala for Machine Learning Chapter 8 Kernel Models and Support Vector Machines
		 */
final class NuSVCFormulation(nu: Double, rho: Double) extends SVMFormulation {
	require(nu >= NU_LIMITS._1 && nu <= NU_LIMITS._2, 
			s"NuSVCFormulation: Nu penalty factor $nu is out of range")  
  
		/**
		 * <p>Initialize the LIBSVM parameters configuration</p>
		 * @param param svm_parameter LIBSVM instance to initialize
		 * @throws IllegalArgumentException if param is undefined.
		 */
	override def update(param: svm_parameter): Unit = {
		require(param != null, "NuSVCFormulation.update Undefined LIBSVM svm parameter class")
  	 	 
		param.svm_type = svm_parameter.NU_SVC
		param.nu = nu
		param.p =rho
	}
			/**
		 * Textual description of the NU-formulation for the Support Vector Regression.
		 */
	override def toString: String = s"Nu-SVC with nu= $nu, rho= $rho"
}


		/**
		 * <p>Class to initialize the type of SVM to a one class support vector classifier for outliers.
		 * </p>
		 * @constructor Create a 1-SVC/Nu formulation
		 * @throws IllegalArgumentException if nu is out of range [0,1]
		 * @param nu normalized penalty factor
		 * @author Patrick Nicolas
		 * @since April 29, 2014
		 * @note Scala for Machine Learning Chapter 8 Kernel Models and Support Vector Machines
		 */
final class OneSVCFormulation(nu: Double) extends SVMFormulation {
	require(nu >= NU_LIMITS._1 && nu <= NU_LIMITS._2, s"nu penalty factor $nu is out of range")  
  
		/**
		 * <p>Initialize the LIBSVM parameters configuration</p>	 
		 * @param param svm_parameter LIBSVM instance to initialize	
		 * @throws IllegalArgumentException if param is undefined.
		 */
	override def update(param: svm_parameter): Unit = {
		require(param != null, "OneSVCFormulation.update Cannot Undefined LIBSVM svm parameter class")
  	 
		param.svm_type = svm_parameter.ONE_CLASS
		param.nu = nu
	}
		/**
		 * Textual description of the formulation for the one class SVM classifier
		 */
	override def toString: String = s"One-class SVC with nu= $nu"
}


		/**
		 * <p>Class to initialize the type of SVM to the epsilon formulation of the support vector 
		 * regressions.</p>
		 * @constructor Create a epsilon- SVR formulation
		 * @throws IllegalArgumentException if nu is out of range [0,1]
		 * @param c  C-penalty or inverse regularization factor.
		 * @param epsilon epsilon factor for the support vector regression.
		 * @author Patrick Nicolas
		 * @since April 29, 2014
		 * @note Scala for Machine Learning Chapter 8 Kernel models and support vector machines
		 */
final class SVRFormulation(c: Double, epsilon: Double) extends SVMFormulation { 
	require(c >= C_LIMITS._1 && c <= C_LIMITS._2, 
			s"SVRFormulation C penalty factor $c is out of range")
	require(epsilon >= EPSILON_LIMITS._1 && epsilon <= EPSILON_LIMITS._2, 
			s"SVRFormulation Epsilon factor $epsilon is out of range") 
   
		/**
		 * <p>Initialize the LIBSVM parameters configuration</p>
		 * @param param svm_parameter LIBSVM instance to initialize
		 * @throws IllegalArgumentException if param is undefined.
		 */
	override def update(param: svm_parameter): Unit = {
		require(param != null, "SVRFormulation.update Undefined LIBSVM svm parameter class")
  	 
		param.svm_type = svm_parameter.EPSILON_SVR
		param.C = c
		param.p = epsilon
	}
	
		/**
		 * Textual description of the formulation for the Support Vector Regression.
		 */
	override def toString: String = s"Epsilon-SVR with C = $c, epsilon= $epsilon"
}


// --------------------------- EOF ------------------------------------------
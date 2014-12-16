/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.2
 */
package org.scalaml.supervised.svm

import libsvm._



		/**
		 * <p>Protected class that encapsulates the execution parameters for SVM training. The
		 * class are instantiated by the companion object.<br>
		 * This implementation uses the LIBSVM library: http://www.csie.ntu.edu.tw/~cjlin/libsvm/</p>
		 * @constructor Create SVM execution configuration with the following parameters
		 * @throws IllegalArgumentException if the cache, convergence criteria or number of folds are 
		 * incorrectly specified.
		 * @param cacheSize Size of the cache used in LIBSVM to preserve intermediate computation 
		 * during training.
		 * @param eps Convergence Criteria to exit the training cycle
		 * @param nFolds Number of folds used in K-fold validation of the SVM model. Value -1 
		 * indicates no validation
		 * 
		 * @author Patrick Nicolas
		 * @since April 28, 2014
		 * @note Scala for Machine Learning Chapter 8 Kernel models and support vector machines.
 		 */
protected class SVMExecution(cacheSize: Int, val eps: Double, val nFolds: Int) extends SVMConfigItem {
	import SVMExecution._
	
	check(cacheSize, eps, nFolds)
	override def update(param: svm_parameter): Unit = {
		param.cache_size = cacheSize
		param.eps = eps
	}

	override def toString: String = s"\nCache size: $cacheSize eps: $eps"
}


		/**
		 * <p>Companion object to the SVMExecution class. The singleton
		 * is used to define the constructors and validate their input parameters.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since April 28, 2014
		 * @note Scala for Machine Learning Chapter 8 Kernel models and support vector machines.
		 */
object SVMExecution {
	private val DEFAULT_CACHE_SIZE = 1024
	private val DEFAULT_EPS = 1e-5

	private val MAX_CACHE_SIZE = 1<<16
	private val EPS_LIMITS = (1e-10, 0.35)

		/**
		 * Default constructor for the SVMExecution class
		 * @param cacheSize Size of the cache used in LIBSVM to preserve intermediate computation 
		 * during training.
		 * @param eps Convergence Criteria to exit the training cycle
		 * @param nFolds Number of folds used in K-fold validation of the SVM model. Value -1 indicates 
		 * no validation
		 */
	def apply(cacheSize: Int, eps: Double, nFolds: Int): SVMExecution = 
		new SVMExecution(cacheSize, eps,nFolds)

	
		/**
		 * Constructor for the SVMExecution class with a default cache size
		 * @param cacheSize Size of the cache used in LIBSVM to preserve intermediate computation 
		 * during training.
		 * @param eps Convergence Criteria to exit the training cycle
		 * @param nFolds Number of folds used in K-fold validation of the SVM model. Value -1 indicates 
		 * no validation
		 */
	def apply(eps: Double, nFolds: Int): SVMExecution = 
		new SVMExecution(DEFAULT_CACHE_SIZE, eps, nFolds)
	
		/**
		 * Constructor for the SVMExecution class with a default cache size and no validation fold
		 * @param cacheSize Size of the cache used in LIBSVM to preserve intermediate computation 
		 * during training.
		 * @param eps Convergence Criteria to exit the training cycle
		 */
	def apply(eps: Double): SVMExecution = new SVMExecution(DEFAULT_CACHE_SIZE, eps, -1)

		/**
		 * Constructor for the SVMExecution class with a default cache size, default eps and no 
		 * validation fold
		 */
	def apply: SVMExecution = new SVMExecution(DEFAULT_CACHE_SIZE, DEFAULT_EPS, -1)

	
	private def check(cacheSize: Int, eps: Double, nFolds: Int): Unit = {
		require(cacheSize >= 0 && cacheSize < MAX_CACHE_SIZE, 
				s"SVMExecution.check cache size $cacheSize is out of range")
		require(eps > EPS_LIMITS._1 && eps < EPS_LIMITS._2, 
				s"SVMExecution.check eps $eps is out of range")
		require(nFolds == -1 || (nFolds > 0 && nFolds <= 10), 
				s"SVMExecution.check Number of folds for validation $nFolds is out of range")
	}
}


// --------------------------- EOF ------------------------------------------
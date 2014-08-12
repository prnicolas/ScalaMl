/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.supervised

import org.scalaml.core.XTSeries


	/**
	 * <p>Trait that defined the interface to supervised learning algorithm.
	 * The trait requires developers to create a validation routine for parameterized
	 * multidimensional time series of tuple (observation, class label).</p>
	 * @author Patrick Nicolas
	 * @since March 4, 2014
	 * @note Scala for Machine Learning
	 */
trait Supervised[T] {
	
		/**
		 * validation method for supervised learning algorithm
		 * @param xt parameterized multidimensional time series of tuple (observation, class label)
		 * @param tpClass index of the class that contains the True positive labels
		 * @return F1 measure
		 */
   def validate(xt: XTSeries[(Array[T], Int)], tpClass: Int): Double
   def crossValidation: Array[Double] = null
}


	/**
	  * <p>Define the configuration trait for the configuration of all the supervised
	  * learning algorithms.</p>
	  */
trait Config


	/**
	  * <p>Define the model for the configuration of all the supervised
	  * learning algorithms. The trait is used to record any information
	  * relevant to the training of the model.</p>
	  */
trait Model {
  var status: String = "Ready"
}


// --------------------------------  EOF ------------------------------------------
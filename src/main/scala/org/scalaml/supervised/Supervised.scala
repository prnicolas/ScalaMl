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
package org.scalaml.supervised

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl.DblVector

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
   def validate(xt: XTSeries[(Array[T], Int)], tpClass: Int): Option[Double]
   def crossValidation: Option[DblVector] = None
}


// --------------------------------  EOF ------------------------------------------
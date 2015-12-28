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
 * Version 0.99
 */
package org.scalaml.supervised

import scala.util.Try

import org.scalaml.core.Types.ScalaMl.{XVSeries, DblArray}
import org.scalaml.core.ITransform
import org.scalaml.validation.Validation

	/**
	 * Trait that defined the interface to supervised learning algorithm.
	 * The trait requires developers to create a validation routine for parameterized
	 * multidimensional time series of tuple (observation, class label).
	 * @tparam T type of elements in the time series
	 * @tparam U type of the input data in the explicit transform
	 * @author Patrick Nicolas
	 * @since 0.98 March 4, 2014
	 * @version 0.98.2
	 * @see Scala for Machine Learning
	 */
trait Supervised[T, V] {
	self: ITransform[V] =>
		/**
		 * validation method for supervised learning algorithm
		 * @param xt parameterized multidimensional time series of tuple (observation, class label)
		 * @param expected values or index of the class that contains the true positive labels
		 * @return F1 measure
		 */
	def validate(xt: XVSeries[T], expected: Vector[V]): Try[Double]
	def crossValidation: Option[DblArray] = None
}


// --------------------------------  EOF ------------------------------------------
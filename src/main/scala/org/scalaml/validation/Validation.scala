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
package org.scalaml.validation

import org.scalaml.util.MapUtils.Counter
import org.scalaml.supervised.Supervised
import org.scalaml.core.Types.ScalaMl._



		/**
		 * Generic trait for the validation method that define a single validation scoring method
		 * @author Patrick Nicolas
		 * @since 0.98.2 January 29, 2014
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 2 "Hello World!" / Assessing a model / Validation
		 * @see org.scalaml.stats.FValidation
		 */
trait Validation {
	type LabeledData[T] = (Array[T], Int)
	type ValidationType[T] = Vector[LabeledData[T]]
		/**
		 * Generic computation of the score of the validation of a classifier or clustering model
		 * @return score of the classifier
		 */
	def score: Double
}


		/**
		 * Companion object to the validation 
		 */
/*
object Validation {
  type LabeledData[T] = (Array[T], Int)
  type ValidationType[T] = Vector[LabeledData[T]]
}
* 
*/


	/**
	 * Generic classifier model validation using a F score.
	 * @tparam T Type of features that is view bounded to a Double
	 * @param labeled  Labeled observations for which the features is array of 
	 * element of type T and the labeled values are the class index (Int)
	 * @param predict Run-time classification function of the classifier. It computes the 
	 * prediction for an observation.
	 * @param f  Implicit view bound (conversion of the type of feature to a Double)
	 * @author Patrick Nicolas
	 * @version 0.99
	 * @see Scala for Machine Learning Chap 2 ''Hello World!'' / Assessing a model / Validation
	 */
abstract protected class FValidation[T <: AnyVal](labeled: Vector[(Array[T], Int)])
		(predict: Array[T] => Int)
		(implicit f: T => Double) extends Validation {

	def this(expected: Vector[Int], xt: XVSeries[T])
			(predict: Array[T] => Int)
			(implicit f: T => Double) = 
		this(xt.zip(expected))(predict: Array[T] => Int)
		
	override def score: Double
}



// --------------------  EOF --------------------------------------------------------
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
package org.scalaml.workflow

import org.scalaml.core._FCT
import org.scalaml.stats.Validation
import org.scalaml.core.Design.PipeOperator
import scala.reflect.ClassTag


	/**
	 * <p>Generic data transformation class that process data with input type T
	 * and output type U. The data transform implements the pipe operator by
	 * applying the transform function. A transform function can be a filtering,
	 * smoothing function, a moving average, a classifier.....</p>
	 * @param op pipe operator implementing the function that transform data
	 * @see org.scalaml.core._FCT
	 * @author Patrick Nicolas
	 * @since December 19, 2013
	 * @note Scala for Machine Learning Chapter 2 Hello world! / Designing a workflow / 
	 * Monadic data transformation
	 */
class Transform[-T: ClassTag, +U](op: PipeOperator[T, U]) extends _FCT[Function[T, U]](op.|>)  {	
	def |> : PartialFunction[T, U] = { case t: T => _fct(t)}
}



	/**
	 * <p>Companion object of the Transform class, used to define the identity
	 * transform (zeroTransform),>/p>
	 */
import PipeOperator._
object Transform {
	def zeroTransform[T: ClassTag] = new Transform[T, T](identity[T])
}


// --------------------------------  EOF ----------------------------------------------------
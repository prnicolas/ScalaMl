/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.workflow

import org.scalaml.core._FCT
import org.scalaml.stats.Validation

	/**
	 * <p>Parameterized pipe operator for processing data within a workflow with
	 * a T as input type and U as output type. The trait implements the F# pipe
	 * operator |>.</p>
	 * @author Patrick Nicolas
	 * @since December, 15, 2013
	 */
trait PipeOperator[-T, +U] {
  def |> (data: T): Option[U]
}


object PipeOperator {
	def identity[T] = new PipeOperator[T,T] { override def |> (data: T): Option[T] = Some(data) }
}


	/**
	 * <p>Generic data transformation class that process data with input type T
	 * and output type U. The data transform implements the pipe operator by
	 * applying the transform function. A transform function can be a filtering,
	 * smoothing function, a moving average, a classifier.....</p>
	 * @param op pipe operator implementing the function that transform data
	 * @throws IllegalArgumentException if the transform function is undefined
	 * @see org.scalaml.core._FCT
	 * @author Patrick Nicolas
	 * @since December 19, 2013
	 */
class Transform[-T, +U](val op: PipeOperator[T, U]) extends _FCT[Function[T, Option[U]]](op.|>)  {
	require(op != null, "Cannot create a monadic transform with undefined transform function")
	
	def |>(data: T): Option[U] = _fct(data)
}



	/**
	 * <p>Companion object of the Transform class, used to define the identity
	 * transform (zeroTransform),>/p>
	 */
import PipeOperator._
object Transform {
	def zeroTransform[T] = new Transform[T, T](identity[T])
}



// --------------------------------  EOF ----------------------------------------------------
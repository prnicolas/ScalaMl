/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98.2
 */
package org.scalaml.core

import scala.language.higherKinds


    /**
     * Generic definition of a Monad used as a template for creating transforms.
     * @author Patrick Nicolas
     * @since December 21, 2013 0.98.1
     * @version 0.98.1
     * @see Scala for Machine Learning Chapter 1 Getting started / Monads and higher kinds
     */
 trait _Monad[M[_]] {
   def apply[T](t: T): M[T]
   def map[T, U](m: M[T])(f: T => U): M[U]
   def flatMap[T, U](m: M[T])(f: T => M[U]): M[U] 
 }
    

  /**
   * Generic definition of a data transformation over an input data of type T. The data
   * transformation is defined as partial function from an input of type T to an output
   * of type U. No assumption is made regarding the internal structure/container of the 
   * input (list, array, map, ... ) or the output. The Partial function avoid the need
   * by sub-classes to defined all the condition on the type and value of the input. 
   * A MatchErr is thrown if the value or type of the input value is not supported. 
   * @tParam T type of data input such as time series or observations
   * @param in input data for data transformation
   * 
   * @author Patrick Nicolas
   * @since April 17, 2015  
   * @version 0.99
   * @see Scala for Machine Learning Chapter 2 Hello World!/Designing a workflow / 
   * Monadic data transformation
   */
class Transform[T](val in: T) { 
  
   /**
    * Declaration of the actual data transformation that take an input of type t
    * @tParam U type of the output of the data transformation (time series, observations, 
    * model parameters...)
    * @return A partial function that implement the data transformation T => U
    */
  def |>[U] : PartialFunction[T, U] 
}



object TransformMonad {

		
	import Transform._

		/**
		 * Monadic container which implements/adapter the most commonly used
		 * Scala higher order methods.This class should not be used directly as they
		 * do not validate any method argument and internal state.
		 * @tParam T type parameter for the transform
		 * @param _fct element contained and managed by the monadic wrapper
		 * @constructor Create a monadic container for data transformation.
     * 
		 * @author Patrick Nicolas
		 * @since December 23, 2013 0.98
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 2 Hello World!/Designing a workflow / 
		 * Monadic data transformation
		 */
	implicit class transform2Monad[+T](_fct: Transform[T]) extends _Monad[Transform] {
	
			/**
			 * Access the element of type T contained in this instance
			 * @return element of type T
			 */
	  override def apply: T = _fct.in
			/**
			 * Implementation of the map method
			 * @param  c function that converts from type T to type U
			 */
		override def map[U](c: T => U): Transform[U] = new Transform[U]( c(_fct.in))
			/**
			 * Implementation of flatMap
			 * @param c function that converts from type T to a monadic container of type U
			 */
		override def flatMap[U](f: T => Transform[U]): Transform[U] = f(_fct.in)
			/**
			 * Implementation of filter for the monadic container
			 * @param  p function that test a condition on the element
			 */
		def filter(p: T =>Boolean): Transform[T] = if( p(_fct.in) ) new Transform[T](_fct.in) else zeroTransform(_fct.in)
	}
}




/*
		/**
		 * implementation of the reduce method
		 * @param f reducer/aggregator/accumulator function applied to the element
		 */
	def reduceLeft[U](f: (U,T) => U)(implicit c: T=> U): U = f(c(_fct), _fct)
		/**
		 * implementation of fold
		 * @param f reducer/aggregator/accumulator function applied to the element
		 */
	def foldLeft[U](zero: U)(f: (U, T) => U)(implicit c: T=> U): U =  f(c(_fct), _fct)
		/**
		 * implementation of the foreach loop
		 * @param p immutable method that process the element of the monadic container
		 */
	def foreach(p: T => Unit): Unit = p(_fct)
}
* 
*/


		/**
		 * Companion object for _FCT that define the constructor apply and the zero
		 * value. Its main purpose is to define a constructor and the Zero method.
		 */
object Transform {
		/**
		 * Define the zero value for the FCT monad.
		 * @param fct contained (i.e. data transformation) element
		 * @return an instance of the container with a contained element, fct
		 */
	def zeroTransform[T](fct: T): Transform[T] = new Transform[T](fct)
}


// -------------------------------  EOF -----------------------------------
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
package org.scalaml.core

import scala.language.higherKinds

		/**
		 * <p>Generic definition of a Monad used as a template for creating transforms.</p>
		 * @author Patrick Nicolas
		 * @since December 21, 2013
		 * @note Scala for Machine Learning Chapter 2 Hello World!/Designing a workflow / 
		 * Monadic data transformation
		 */
trait Monad[M[_]] {
	def apply[T](t: T): M[T]
	def map[T,U](m: M[T])(f: T=>U): M[U]
	def flatMap[T,U](m: M[T])(f: T =>M[U]): M[U] 
}


import _FCT._

		/**
		 * <p>Monadic container which implements/adapter the most commonly used
		 * Scala higher order methods.This class should not be used directly as they
		 * do not validate any method argument and internal state.</p>
		 * @param _fct element contained and managed by the monadic wrapper
		 * @constructor Create a monadic container for data transformation.
		 * @author Patrick Nicolas
		 * @since December 23, 2013
		 * @note Scala for Machine Learning Chapter 2 Hello World!/Designing a workflow / 
		 * Monadic data transformation
		 */
class _FCT[+T](val _fct: T) {

		/**
		 * Access the element of type T contained in this instance
		 * @return element of type T
		 */
	def apply: T = _fct
		/**
		 * Implementation of the map method
		 * @param  c function that converts from type T to type U
		 */
	def map[U](c: T => U): _FCT[U] = new _FCT[U]( c(_fct))
		/**
		 * Implementation of flatMap
		 * @param  c function that converts from type T to a monadic container of type U
		 */
	def flatMap[U](f: T =>_FCT[U]): _FCT[U] = f(_fct)
		/**
		 * Implementation of filter for the monadic container
		 * @param  p function that test a condition on the element
		 */
	def filter(p: T =>Boolean): _FCT[T] = if( p(_fct) ) new _FCT[T](_fct) else zeroFCT(_fct)
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


		/**
		 * Companion object for _FCT that define the constructor apply and the zero
		 * value. Its main purpose is to define a constructor and the Zero method.
		 */
object _FCT {
		/**
		 * Define the zero value for the FCT monad.
		 * @param fct contained (i.e. data transformation) element
		 * @return an instance of the container with a contained element, fct
		 */
	def zeroFCT[T](fct: T): _FCT[T] = new _FCT[T](fct)
		
		/**
		 * Generic constructor for the container class.
		 * @param item wrapped in the monadic container
		 * @return an instance of the monadic container.
		 */
	def apply[T](t: T): _FCT[T] = new _FCT[T](t)
}


// -------------------------------  EOF -----------------------------------
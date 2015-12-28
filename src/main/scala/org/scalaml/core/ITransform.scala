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
 * Version 0.99.1
 */
package org.scalaml.core

import org.scalaml.core.functional._Monad

import scala.language.higherKinds
import scala.util.Try

    

  /**
   * Generic definition of a data transformation over an input data of type T. The data
   * transformation is defined as partial function from an input of type T to an output
   * of type U.
   * {{{
   *   		data: Vector[T] => implicit model 
   *     t: T -> | implicit model } -> u: U
   * }}}
   * The Partial function avoid the need define all the condition on the type and value of 
   * the input inj sub-classes 
   * A MatchErr is thrown if the value or type of the input value is not supported. 
   * @tparam T type of element of data input such as time series or observations
   * @param xt input data for data transformation. This is a single or multi-dimensional vector
   * 
   * @author Patrick Nicolas
   * @since 0.99 April 17, 2015  
   * @version 0.99.1.1
   * @see Scala for Machine Learning Chapter 2 "Hello World!" Designing a workflow / 
   * Monadic data transformation
   * @note This data transformation relies on a model which is implicitly extracted or generated
   * from a input data (or time series)
   */
private[scalaml] abstract class ITransform[T](val xt: Vector[T]) {
	type V

		/**
		* Declaration of the actual data transformation that take an input of type t
		* @return A partial function that implement the conversion of data element T => Try[V]
		*/
	def |> : PartialFunction[T, Try[V]]
}


		/**
		 * Object wrapper for the monadic implementation of the implicit data transformation
		 * @author Patrick Nicolas
		 * @version 0.99.1.1
		 */
object ITransformMonad {
	
	private def iTransform[T](data: Vector[T]): ITransform[T] = new ITransform[T](data) {
		override def |> : PartialFunction[T, Try[V]] = { case _ => null.asInstanceOf[Try[V]] }
	}
	
	
	private val iTransformMonad = new _Monad[ITransform] {
		override def unit[T](t: T) = iTransform(Vector[T](t))
		override def map[T, U](m: ITransform[T])(f: T => U): ITransform[U] = 
				iTransform( m.xt.map(f) )
		override def flatMap[T, U](m: ITransform[T])(f: T => ITransform[U]): ITransform[U] = 
				iTransform(m.xt.flatMap(t => f(t).xt))
	}
	
		/**
		 * Monadic container which implements/adapter the most commonly used
		 * Scala higher order methods.This class should not be used directly as they
		 * do not validate any method argument and internal state.
		 * {{{
     *  Use the monadic template   
     *    trait _Monad[M[_]] {
     *      def unit[T](t: T): M[T]
     *      def map[T, U](m: M[T])(f: T => U): M[U]
     *      def flatMap[T, U](m: M[T])(f: T => M[U]): M[U] 
     *   }
     * }}}
		 * @tparam T type parameter for the transform
		 * @param fct element contained and managed by the monadic wrapper
		 * @constructor Create a monadic container for data transformation.
     * 
		 * @author Patrick Nicolas
		 * @since December 23, 2013 0.98
		 * @version 0.99.1.1
		 * @see Scala for Machine Learning Chapter 2 Hello World!/Designing a workflow / 
		 * Monadic data transformation
		 */
	implicit class iTransform2Monad[T](fct: ITransform[T]) {
	
			/**
			 * Access the element of type T contained in this instance
			 * @return element of type T
			 */
		def unit(t: T) = iTransformMonad.unit(t)
			/**
			 * Implementation of the map method
			 * @tparam U type of the output of morphism on element of a data
			 * @param  f function that converts from type T to type U
			 */
		def map[U](f: T => U): ITransform[U] = iTransformMonad.map(fct)(f)
			/**
			 * Implementation of flatMap
			 * @tparam U type of the output of morphism on element of a data
			 * @param f function that converts from type T to a monadic container of type U
			 */
		def flatMap[U](f: T => ITransform[U]): ITransform[U] = iTransformMonad.flatMap(fct)(f)
	  
			/**
			 * Implementation of filter for the monadic container
			 * @param  p function that test a condition on the element
			 */
		def filter(p: T =>Boolean): ITransform[T] = iTransform(fct.xt.filter(p))
	}
	
			/**
		 * Define the zero value for the ''ITransform'' monad.
		 * @tparam T type of the element of the time series
		 * @return an instance of the container with a contained element, fct
		 */
	def zeroTransform[T]: ITransform[T] = iTransform(Vector.empty[T])
}


// -------------------------------  EOF -----------------------------------
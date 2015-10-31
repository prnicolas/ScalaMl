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
package org.scalaml.core.functional

import scala.language.higherKinds



		/**
		 * Generic definition of a Monad used as a template for creating implicit and explicit data
		 * transformations
		 * @tparam M Type of the data transformation or container
		 * @author Patrick Nicolas
		 * @since December 21, 2013 0.98.1
		 * @version 0.98.1
		 * @see Scala for Machine Learning Chapter 1 Getting started / Monads and higher kinds
		 */
trait _Monad[M[_]] {
	def unit[T](t: T): M[T]
	def map[T, U](m: M[T])(f: T => U): M[U]
	def flatMap[T, U](m: M[T])(f: T => M[U]): M[U] 
 }
 

// -------------------------------  EOF -----------------------------------
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
package org.scalaml.workflow

import scala.util.Try
import org.apache.log4j.Logger

import org.scalaml.core.ETransform




		/**
		 * First processing module used in the test workflow. This module is "injected" at run-time
		 * to build a dynamic computation workflow.
		 * @author Patrick Nicolas
		 * @since 0.98.1  December 11, 2013
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 2 Hello world! / Designing a workflow / 
		 * Dependency injection
		 */
trait Sampling[T,A,B] { val sampler: ETransform[T] { type U = A; type V = B} }

		/**
		 * Second processing module used in the workflow. This module is "injected" at run-time
		 * to build a dynamic computation workflow
		 * @author Patrick Nicolas
		 * @since 0.908.1 December 11, 2013
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 2 Hello world! / Designing a workflow / 
		 * 	Dependency injection
		 */
trait Normalization[T,A,B] { val normalizer: ETransform[T] { type U = A; type V = B} }

		/**
		 * Third processing module used in the workflow. This module is "injected" at run-time
		 * to build a dynamic computation workflow
		 * @author Patrick Nicolas
		 * @since 0.98.1 December 11, 2013
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 2 Hello world! / Designing a workflow / 
		 * Dependency injection
		 */
trait Aggregation[T,A,B] { val aggregator: ETransform[T] { type U = A; type V = B} }


		/**
		 * Generic workflow using stackable traits/modules which instances
		 * are initialize at run-time.
		 * @author Patrick Nicolas
		 * @since 0.98.1 December 11, 2013
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 2 Hello world! / Designing a workflow / 
		 * Dependency injection
		 */
class Workflow[T,U,V,W,Z] {
  self: Sampling[T,U,V] with Normalization[T,V,W] with Aggregation[T,W,Z] =>

		def |> (u: U): Try[Z] = for {
		  v <- sampler |> u
		  w <- normalizer |> v
		  z <- aggregator |> w
		} yield z
}




// --------------------------  EOF ----------------------------------------

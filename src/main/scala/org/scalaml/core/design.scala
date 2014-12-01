/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96c
 */
package org.scalaml.core
import scala.reflect.ClassTag

		/**
		 * <p>Package that encapsulate the high level trait used in
		 * supervised and un-supervised learning algorithms.</p>
		 * @author Patrick Nicolas
		 * @since March 4, 2014
		 * @note Scale for Machine Learning Chapter 2 Hello World!
		 */
package object design {

		/**
		 * <p>Define the configuration trait used in the classifiers and optimizers.<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
		 * <b>persists</b>   Abstract value to be defined by sub-classes</span></pre></p>
		 * @author Patrick Nicolas
		 * @since March 4, 2014
		 * @note Scale for Machine Learning Chapter 2 Hello World!
		 */
	trait Config {
		val persists: String
	}
	
		/**
		 * <p>Define the model trait for classification and optimization algorithms.<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
		 *   <b>persists</b>   Abstract value to be defined by sub-classes
		 * </span></pre></p>
		 * @constructor [persists] abstract value to be defined by sub-class
		 * @author Patrick Nicolas
		 * @since March 4, 2014
		 * @note Scala for Machine Learning Chapter 2 Hello World!
		 */
	trait Model { 
		val persists: String
	}
	
	
		/**
		 * <p>Parameterized pipe operator for processing data within a workflow with
		 * a T as input type and U as output type. The trait defines the F# pipe
		 * operator |> which is implemented as a partial function.</p>
		 * @author Patrick Nicolas
		 * @since December, 15, 2013
		 * @note Scala for Machine Learning Chapter 2 Hello World! / Designing a workflow / The pipe operator
		 */
	trait PipeOperator[-T, +U] {  def |> : PartialFunction[T, U] }

		/**
		 * Companion object for the PipeOperator trait. It used to 
		 * define the identity (or zero) element as an instance that
		 * implement the |> operator as an identity function.
		 */
	object PipeOperator {
		def identity[T: ClassTag] = new PipeOperator[T,T] { 
			override def |> : PartialFunction[T, T] = { case t: T => t  }
		}
	}
}

// --------------------------------------------------  EOF ----------------------------------------
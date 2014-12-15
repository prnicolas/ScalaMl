/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.3
 */
package org.scalaml.core

import scala.reflect.ClassTag
import scala.util.{Try, Success, Failure}

import org.apache.log4j.Logger

import org.scalaml.util.DisplayUtils

		/**
		 * <p>Package that encapsulate the high level trait used in
		 * supervised and un-supervised learning algorithms.</p>
		 * @author Patrick Nicolas
		 * @since March 4, 2014
		 * @note Scale for Machine Learning Chapter 2 Hello World!
		 */
package object design {
	import org.scalaml.util.FileUtils

		/**
		 * <p>Define the configuration trait used in the classifiers and optimizers.</p>
		 * @author Patrick Nicolas
		 * @since March 4, 2014
		 * @note Scale for Machine Learning Chapter 2 Hello World!
		 */
	trait Config {
			
		/**
		 * Name of the file that persists the model
		 */
		protected val persists: String
		
		/**
		 * Read the configuration of this object from a file <b>persists</b>
		 * @param content Configuration to write into file
		 * @return Configuration parameters if successful, None otherwise
		 */
		def <<(content: String): Option[String] = FileUtils.read(persists, getClass.getName)
	
		/**
		 * Write the configuration parameters associated to this object into a file
		 * @param content to write into a file
		 * @return true if the write operation is successful, false otherwise
		 */
		def >>(content: String) : Boolean = FileUtils.write(content, persists, getClass.getName)
	}
	
		/**
		 * <p>Define the model trait for classification and optimization algorithms.</p>
		 * @author Patrick Nicolas
		 * @since March 4, 2014
		 * @note Scala for Machine Learning Chapter 2 Hello World!
		 */
	trait Model {
			
		/**
		 * Name of the file that persists the model
		 */
		protected val persists: String
					
		/**
		 * Read the model of this object from a file <b>persists</b>
		 * @param content Model description to write into file
		 * @return Model parameters if successful, None otherwise
		 */
		def <<(content: String): Option[String] = FileUtils.read(persists, getClass.getName)

		/**
		 * Write the configuration parameters associated to this object into a file
		 * @param content to write into a file
		 * @return true if the write operation is successful, false otherwise
		 */
		def >>(content: String) : Boolean = FileUtils.write(content, persists, getClass.getName)
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
		 * @author Patrick Nicolas
		 * @since March 4, 2014
		 * @note Scala for Machine Learning Chapter 2 Hello World!
		 */
	object PipeOperator {
		def identity[T: ClassTag] = new PipeOperator[T,T] { 
			override def |> : PartialFunction[T, T] = { case t: T => t  }
		}
	}
}

// --------------------------------------------------  EOF ----------------------------------------
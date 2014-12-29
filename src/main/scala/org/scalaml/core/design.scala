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

import scala.reflect.ClassTag
import scala.util.{Try, Success, Failure}

import org.apache.log4j.Logger

import org.scalaml.util.DisplayUtils

		/**
		 * <p>Package that encapsulate the high level trait used in
		 * supervised and unsupervised learning algorithms.</p>
		 * @author Patrick Nicolas
		 * @since March 4, 2014
		 * @note Scale for Machine Learning Chapter 2 Hello World!
		 */
object Design {
	import org.scalaml.util.FileUtils

		/**
		 * <p>Define the configuration trait used in the classifiers and optimizers. The configuration
		 * parameters is loaded using <b>Config.read</b> method invoked by one of the constructor of 
		 * the configuration. The configuration parameters are saved into file by overriding the 
		 * method >>.
		 * </p>
		 * @author Patrick Nicolas
		 * @since April 11, 2014
		 * @note Scale for Machine Learning Chapter 2 Hello World!
		 */
	trait Config {
			/**
		 * Write the configuration parameters associated to this object into a file
		 * @param content to write into a file
		 * @return true if the write operation is successful, false otherwise
		 */
		protected def write(content: String): Boolean  = 
				FileUtils.write(content, Config.RELATIVE_PATH, getClass.getSimpleName)
		
		/**
		 * <p>Write the configuration parameters associated to this object.</p>
		 * @return true if the write operation is successful, false otherwise
		 */
		def >> : Boolean = false
	}
	
		/**
		 * Companion singleton to the Config trait. It is used to define the simple read 
		 * method to load the config parameters from file and instantiate the configuration
		 * @author Patrick Nicolas
		 * @since April 11, 2014
		 * @note Scale for Machine Learning Chapter 2 Hello World!
		 */
	object Config {
		private val RELATIVE_PATH = "configs/"
		/**
		 * Read this algorithm configuration parameters from a file defined as 
		 * <b>configs/className</b>
		 * @param className  file containing the configuration parameters
		 * @return Configuration parameters as a comma delimited field of string  if successful, 
		 * None otherwise
		 */
		def read(className: String): Option[String] = FileUtils.read(RELATIVE_PATH, className)
	}
	
	
	
		/**
		 * <p>Define the model trait for classification and optimization algorithms.</p>
		 * @author Patrick Nicolas
		 * @since March 4, 2014
		 * @note Scala for Machine Learning Chapter 2 Hello World!
		 */
	trait Model {
		/**
		 * Write the model parameters associated to this object into a file
		 * @param content to write into a file
		 * @return true if the write operation is successful, false otherwise
		 */
		protected def write(content: String): Boolean  = 
				FileUtils.write(content, Model.RELATIVE_PATH, getClass.getSimpleName)
		
			/**
			 * This operation or method has to be overwritten for a model to be saved into a file
			 * @return It returns true if the model has been properly saved, false otherwise
			 */
		def >> : Boolean = false
	}

		/**
		 * Companion singleton to the Model trait. It is used to define the simple read 
		 * method to load the model parameters from file.
		 * @author Patrick Nicolas
		 * @since March 4, 2014
		 * @note Scala for Machine Learning Chapter 2 Hello World!
		 */
	object Model {
		private val RELATIVE_PATH = "models/"
		/**
		 * Read this model parameters from a file defined as <b>models/className</b>
		 * @param className  file containing the model parameters
		 * @return Model parameters as a comma delimited string if successful, None otherwise
		 */
		def read(className: String): Option[String] = FileUtils.read(RELATIVE_PATH, className)
	}
	
	
		/**
		 * <p>Parameterized pipe operator for processing data within a workflow with
		 * a T as input type and U as output type. The trait defines the F# pipe
		 * operator |> which is implemented as a partial function.</p>
		 * 
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
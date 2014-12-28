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
		 * parameters are retrieved  using the method <<. The configuration parameters are saved
		 * into file by overriding the method >> as follows:<br> 
		 *  val nb = BinNaiveBayes
		 * </p>
		 * @author Patrick Nicolas
		 * @since March 4, 2014
		 * @note Scale for Machine Learning Chapter 2 Hello World!
		 */
	trait Config {
					
		/**
		 * Name of the file that persists this configuration
		 */
		protected val persists: String
		
		/**
		 * Read the configuration of this object from a file <b>persists</b>
		 * @return Configuration parameters if the file exists and the configuration has
		 * been saved successfully, None otherwise
		 */
		def << : Option[String] = FileUtils.read(persists, getClass.getName)
	
		/**
		 * Write the configuration parameters associated to this object into a file
		 * @param content to write into a file
		 * @return true if the write operation is successful, false otherwise
		 */
		protected def >>(content: String) : Boolean = FileUtils.write(content, persists, getClass.getName)
		
		/**
		 * <p>Write the configuration parameters associated to this object.</p>
		 * @return true if the write operation is successful, false otherwise
		 */
		protected def >> : Boolean = false
	}
	
		/**
		 * <p>Define the model trait for classification and optimization algorithms.</p>
		 * @author Patrick Nicolas
		 * @since March 4, 2014
		 * @note Scala for Machine Learning Chapter 2 Hello World!
		 */
	trait Model {
		/**
		 * Write the configuration parameters associated to this object into a file
		 * @param content to write into a file
		 * @return true if the write operation is successful, false otherwise
		 */
		protected def write(content: String): Boolean  = 
				FileUtils.write(content, Model.MODEL_RELATIVE_PATH, getClass.getSimpleName)
		
			/**
			 * This operation or method has to be overwritten for a model to be saved into a file
			 * @return It returns true if the model has been properly saved, false otherwise
			 */
		def >> : Boolean = false
	}

		/**
		 * Companion singleton to the Model trait. It is used to define the simple read 
		 * method to load the model parameters from file.
		 */
	object Model {
		private val MODEL_RELATIVE_PATH = "model/"
		/**
		 * Read this model parameters from a file defined as <b>model/className</b>
		 * @param className  file containing the model parameters
		 * @return Model parameters as string if successful, None otherwise
		 */
		def read(className: String): Option[String] = FileUtils.read(MODEL_RELATIVE_PATH, className)
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
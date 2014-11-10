/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95c
 */
package org.scalaml.core
import scala.reflect.ClassTag

		/**
		 * <p>Package that encapsulate the high level trait used in
		 * supervised and un-supervised learning algorithms.</p>
		 * @author Patrick Nicolas
		 * @since March 4, 2014
		 * @note Scale for Machine Learning
		 */
package object design {

	/**
	  * <p>Define the configuration trait used in the classifiers and optimizers.</p>
	  * @constructor [persists] abstract value to be defined by sub-class
	  * @author Patrick Nicolas
	  * @since March 4, 2014
	  * @note Scale for Machine Learning
	  */
  trait Config {
    val persists: String
  }
	
	
	/**
	  * <p>Define the model trait for classification and optimization algorithms.</p>
	  * @constructor [persists] abstract value to be defined by sub-class
	  * @author Patrick Nicolas
	  * @since March 4, 2014
	  * @note Scala for Machine Learning
	  */
  trait Model { 
    val persists: String
  }
	
	
		/**
		 * <p>Parameterized pipe operator for processing data within a workflow with
		 * a T as input type and U as output type. The trait implements the F# pipe
		 * operator |> which is implemented as a partial function.</p>
		 * @author Patrick Nicolas
		 * @since December, 15, 2013
		 * @note Scala for Machine Learning
		 */
  trait PipeOperator[-T, +U] {  def |> : PartialFunction[T, U] }

  
  object PipeOperator {
	 def identity[T: ClassTag] = new PipeOperator[T,T] { 
	   override def |> : PartialFunction[T, T] = { case t: T => t  }
	 }
   }
}

// --------------------------------------------------  EOF ----------------------------------------
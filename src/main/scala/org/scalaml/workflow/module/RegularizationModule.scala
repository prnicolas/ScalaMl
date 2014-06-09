/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.workflow.module



	/**
	 * Factory for specialized Furniture (i.e. Bathroom furniture). This traits
	 * illustrates the second option to create a component dynamically using module or 
	 * factory inheritance.
	 */
trait RegularizationModule[T] extends PreprocessingModule[T] {   
   class Regularization[T](val category: String) extends Preprocessing[T]( (t:T) => Math.exp(t.asInstanceOf[Double]).asInstanceOf[T], 1e-3) {
      override def process(x: Array[T]): Array[T] = (Array[Double](0.6, 1.7)).asInstanceOf[Array[T]]
   }
}


// ------------------------  EOF ------------------------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.workflow.module

import scala.Array.fallbackCanBuildFrom


	/**
	 * Module or factory for furniture of type Furniture or sub-types. This trait 
	 * illustrates the first option of creating dynamic component dynamically using class
	 * inheritance(i.e. Furniture <- PatioFurniture
	 */
trait PreprocessingModule[T] {
  val preprocessor: Preprocessing[T]
  
  class Preprocessing[T](val f: T =>T, val errorTolerance: Double) {
     def process(values: Array[T]): Array[T] = values.map( f ).asInstanceOf[Array[T]]
  }
  
  class Filtering[T](val g: T => T, val decay: (Int, T) => T, val rate: Int) 
                                                  extends Preprocessing(g, 1e-3) {
     override def process(values: Array[T]): Array[T] = super.process(values).map( decay(rate, _)).asInstanceOf[Array[T]]
  }
}



// ----------------------------------  EOF -------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.workflow.module



import org.scalaml.core.XTSeries

trait ValidationModule[T] {
 val validation: Validation[T]
  
  class Validation[T](val distance: Array[T] =>Double, val k: Int, val eps: Double) {
     def process(input: XTSeries[T]): Int = -1
  }
}

// -----------------------  EOF -----------------------------------
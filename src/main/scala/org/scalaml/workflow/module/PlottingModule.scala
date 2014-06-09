/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.workflow.module



trait PlottingModule[T] {
  val plotting: Plotting[T]
  
  class Plotting[T]
}

// ------------------------  EOF ---------------------------------------------------
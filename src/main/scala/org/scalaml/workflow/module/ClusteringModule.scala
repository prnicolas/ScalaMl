/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.workflow.module


import org.scalaml.core.XTSeries

	/**
	 * Factory for Appliance.
	 */
trait ClusteringModule[T] {
  val clustering: Clustering[T]
  
  class Clustering[T](val distance: Array[T] =>Double, val k: Int, val eps: Double) {
     def process(input: XTSeries[T]): Int = -1
  }
}

// ---------------------------------------  EOF ------------------------------------------------------
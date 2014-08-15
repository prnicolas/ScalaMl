/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.workflow.module


import org.scalaml.core.XTSeries
import org.scalaml.workflow.PipeOperator
import org.scalaml.unsupervised.clustering.Cluster
import org.scalaml.core.Types.ScalaMl.DblVector

	/**
	 * Factory for Appliance.
	 */
trait ClusteringModule[T] { 
  type EMOutput = List[(Double, DblVector, DblVector)]
  val clustering: Clustering[T] 
  
  abstract class Clustering[T <% Double] {
  	 def execute(xt: XTSeries[Array[T]]): Unit 
  }
  
  
  final class KMeans[T <% Double](val K: Int, val maxIters: Int, val distance: (DblVector, Array[T]) => Double)(implicit order: Ordering[T], m: Manifest[T]) 
                                   extends Clustering[T] with PipeOperator[XTSeries[Array[T]], List[Cluster[T]]] { 
  	
  	  override def |> (xt: XTSeries[Array[T]]): Option[List[Cluster[T]]] = { 
  	    None
  	  }
  	  override def execute(xt: XTSeries[Array[T]]): Unit =  this |> xt match {
  	  	 case Some(clusters) => clusters.foreach( println )
  	  	 case None => println("failed to create EM clusters")
  	  }
  }
  
  final class MultivariateEM[T <% Double](val K: Int) extends Clustering[T] with PipeOperator[XTSeries[Array[T]], EMOutput] {
  	  override def |> (xt: XTSeries[Array[T]]): Option[EMOutput] = { None }
  	  
  	  override def execute(xt: XTSeries[Array[T]]): Unit = this |> xt match {
  	  	  case Some(emOutput) => println(emOutput.toString)
  	  	  case None => println("failed to create EM clusters")
  	  }
  }
}

// ---------------------------------------  EOF ------------------------------------------------------
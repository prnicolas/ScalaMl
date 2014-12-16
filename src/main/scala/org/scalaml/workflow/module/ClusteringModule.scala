/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.2
 */
package org.scalaml.workflow.module


import org.scalaml.core.XTSeries
import org.scalaml.core.design.PipeOperator
import org.scalaml.unsupervised.clustering.Cluster
import org.scalaml.core.Types.ScalaMl.DblVector

	/**
	 * <p>Clustering module used to instantiate a clustering component in a workflow.</p>
	 */
trait ClusteringModule[T] { 
	type EMOutput = List[(Double, DblVector, DblVector)]
	val clustering: Clustering[T] 
  
	abstract class Clustering[T <% Double] {
		def execute(xt: XTSeries[Array[T]]): Unit 	
	}
  
	import Cluster._
	final class KMeans[T <% Double](
			K: Int, 
			maxIters: Int, 
			distance: (DblVector, Array[T]) => Double)
			(implicit order: Ordering[T], m: Manifest[T]) extends Clustering[T] 
					with PipeOperator[XTSeries[Array[T]], List[Cluster[T]]] { 
  	
		override def |> : PartialFunction[XTSeries[Array[T]], List[Cluster[T]]] = { case _ => List.empty }
  	  
		override def execute(xt: XTSeries[Array[T]]): Unit =  this |> xt
  }
  
  final class MultivariateEM[T <% Double](K: Int) extends Clustering[T] 
  		with PipeOperator[XTSeries[Array[T]], EMOutput] {
    
  	override def |> : PartialFunction[XTSeries[Array[T]], EMOutput] = { case _ => List.empty  }
  	override def execute(xt: XTSeries[Array[T]]): Unit = this |> xt 
  }
}

// ---------------------------------------  EOF ------------------------------------------------------
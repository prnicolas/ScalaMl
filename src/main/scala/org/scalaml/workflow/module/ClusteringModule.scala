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
package org.scalaml.workflow.module

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.core.Design.PipeOperator
import org.scalaml.unsupervised.clustering.Cluster
import org.scalaml.unsupervised.clustering.KMeans
import org.apache.log4j.Logger
import org.scalaml.util.DisplayUtils

	/**
	 * <p>Clustering module used to instantiate a clustering component in a workflow. The
	 * module can contain an arbitrary number of clustering algorithms. This
	 * class illustrates the injection dependency capabilities of Scala</p>
	 * 
	 * @author Patrick Nicolas
	 * @since January 22, 2014
	 * @note Scala for Machine Learning Chapter 2 Hello World! Designing a workflow
	 */
trait ClusteringModule[T] { 
	private val logger = Logger.getLogger("PreprocessingModule")
	
		/**
		 * Clustering algorithm to be defined at run-time
		 */
	val clustering: Clustering[T] 
 
		/**
		 * Base class for all clustering algorithm
		 */
	abstract class Clustering[T <% Double] {

		/**
		 * Process a time series using this specific clustering algorithm
		 * @param xt time series
		 */
		def execute(xt: XTSeries[Array[T]]): Unit 	
	}

		/**
		 * K-Means clustering algorithm. This class a simple wrapper to the actual
		 * K-means implementation defined in chapter 4
		 * @constructor Create a wrapper for K-means
		 * @see org.scalaml.unsupervised.clustering.KMeans
		 * @param K Number of clusters
		 * @param maxIters Maximum number of iterations allowed for the generation of clusters.
		 * @param distance Metric used in computing distance between data points.
		 */
	final class KMeans[T <% Double](
			K: Int, 
			maxIters: Int, 
			distance: (DblVector, Array[T]) => Double)
			(implicit order: Ordering[T], m: Manifest[T]) extends Clustering[T] { 

		import org.scalaml.unsupervised.clustering.KMeans
		private[this] val kmeans = KMeans(K, maxIters,distance)
		
		/**
		 * Apply K-means to the time series
		 * @param xt time series
		 */
		override def execute(xt: XTSeries[Array[T]]): Unit =  {
			try {
				val clusters = kmeans |> xt
				DisplayUtils.show(clusters, logger)
			}
			catch {
				case e: MatchError => {
					val errMsg = s"${e.getMessage} caused by ${e.getCause.toString}"
					DisplayUtils.error(s"ClusteringModule.Kmeans $errMsg", logger)
			  }
			  case e: Throwable => DisplayUtils.error("ClusteringModule.Kmeans", logger, e)
			}
		}
	}

	
		/**
		 * <p>Multivariate Expectation-Maximization algorithm. This class is a wrapper
		 * for the Multivariate expectation-maximization algorithm introduced in Chapter 4.</p>
		 * @constructor Instantiate a Multivariate Expectation Maximization for time series of data 
		 * point of type Array{T]. 
		 * @see org.scalaml.unsupervised.em.MultivariateEM
		 * @throws IllegalArgumentException if K is out of range
		 * @param K Number of clusters used in the Expectation-Maximization algorithm.
		 */
	final class MultivariateEM[T <% Double](K: Int) extends Clustering[T] {
		import org.scalaml.unsupervised.em.MultivariateEM
		private[this] val em = MultivariateEM[T](K)
		/**
		 * Apply Expectation-Maximization algorithm to the time series
		 * @param xt input time series
		 */
		override def execute(xt: XTSeries[Array[T]]): Unit = {
			try {
				val results = em |> xt 
				DisplayUtils.show(results, logger)
			}
			catch {
				case e: MatchError => {
					val errMsg = s"${e.getMessage} caused by ${e.getCause.toString}"
					DisplayUtils.error(s"ClusteringModule.MultivariateEM $errMsg", logger)
				}
				case e: Throwable => DisplayUtils.error("ClusteringModule.Kmeans", logger, e)
			}
		}
	}
}

// ---------------------------------------  EOF ------------------------------------------------------
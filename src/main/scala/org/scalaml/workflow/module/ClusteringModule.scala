/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99.1
 */
package org.scalaml.workflow.module

import org.scalaml.stats.XTSeries

import org.scalaml.core.Types.ScalaMl.{DblArray, XVSeries}
import org.scalaml.unsupervised.clustering.Cluster
import org.scalaml.unsupervised.clustering.{KMeansConfig, KMeans}
import org.apache.log4j.Logger


	/**
	 * Clustering module used to instantiate a clustering component in a workflow. The
	 * module can contain an arbitrary number of clustering algorithms. This
	 * class illustrates the injection dependency capabilities of Scala
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
	trait Clustering[U] {

		/**
		 * Process a time series using this specific clustering algorithm
		 * @param xt time series
		 */
		def execute(xt: Array[U]): Unit
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
	final class KMeans[U <: AnyVal](
			K: Int, 
			maxIters: Int, 
			distance: (DblArray, Array[U]) => Double,
			xt: XVSeries[U])
			(implicit m: Manifest[U], num: Numeric[U], f: U => Double) extends Clustering[U] {

		import org.scalaml.util.DisplayUtils._
	  import org.scalaml._

		private[this] val kmeans = unsupervised.clustering.KMeans[U](K, maxIters, distance, xt)
		
		/**
		 * Apply K-means to the time series
		 * @param x time series
		 */
		override def execute(x: Array[U]): Unit =  {
			try {
				val clusters = kmeans |> x
				show(clusters, logger)
			}
			catch {
				case e: MatchError =>
					val errMsg = s"${e.getMessage()} caused by ${e.getCause.toString}"
					error(s"ClusteringModule.Kmeans $errMsg", logger)
			  case e: Throwable => error("ClusteringModule.Kmeans", logger, e)
			}
		}
	}

	
		/**
		 * Multivariate Expectation-Maximization algorithm. This class is a wrapper
		 * for the Multivariate expectation-maximization algorithm introduced in Chapter 4.
		 * @constructor Instantiate a Multivariate Expectation Maximization for time series of data 
		 * point of type Array{T]. 
		 * @see org.scalaml.unsupervised.em.MultivariateEM
		 * @throws IllegalArgumentException if K is out of range
		 * @param K Number of clusters used in the Expectation-Maximization algorithm.
		 */
	final class MultivariateEM[T <: AnyVal](K: Int, xt: XVSeries[T])(implicit f: T =>Double) 
			extends Clustering[T] {

		import org.scalaml.util.DisplayUtils._
		private[this] val em = org.scalaml.unsupervised.em.MultivariateEM[T](K, xt)
		/**
		 * Apply Expectation-Maximization algorithm to the time series
		 * @param xt input time series
		 */
		override def execute(xt: Array[T]): Unit = {
			try {
				val results = em |> xt 
				show(results, logger)
			}
			catch {
				case e: MatchError =>
					val errMsg = s"${e.getMessage()} caused by ${e.getCause.toString}"
					error(s"ClusteringModule.MultivariateEM $errMsg", logger)
				case e: Throwable => error("ClusteringModule.Kmeans", logger, e)
			}
		}
	}
}

// ---------------------------------------  EOF ------------------------------------------------------
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
 * Version 0.99
 */
package org.scalaml.unsupervised.clustering

   // Scala standard library
import scala.annotation.{implicitNotFound, tailrec}

import scala.util.{Try, Success, Failure}
   // 3rd party libraries
import org.apache.log4j.Logger

   // Scala for machine learning classes
import org.scalaml.stats.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.ITransform
import org.scalaml.unsupervised.Distance
import org.scalaml.util.LoggingUtils._
import ScalaMl._, Cluster._, XTSeries._, KMeans._


	/**
	 * Simple case class for the configuration of K-means
	 * @param K  Number of clusters
	 * @param maxIters Maximum number of iterations allowed in the minimization of the
	 * reconstruction error
	 * @author Patrick Nicolas
	 * @since 0.99 July 24, 2015
	 */
case class KMeansConfig(val K: Int, maxIters: Int)

		/**
		 * Class that implements the KMeans++ algorithm for which the centroids
		 * are initialized at mid point of K segments of data points after the data points
		 * are ordered by their variance.
		 * {{{

		 *  Minimize the reconstruction error SUM all clusters [SUM d(x(i), m(k)] x(i) 
		 *  belonging to Cluster k with center m(k)
		 *  }}}
		 * @tparam T type of the elements of the data set
		 * @constructor Initiate a K-means algorithm with a predefined number of cluster, maximum 
		 * number of iterations and a distance metric. 
		 * @throws IllegalArgumentException if the number of clusters or the maximum number of 
		 * iterations is out of range or if the distance metric is undefined.
		 * @throws implicitNotFoundException if the ordering instance is not implicitly defined.
		 * @param config Configuration for the execution of the KMeans algorithm
		 * @param m Implicit declaration of manifest of type '''T''' to overcome Java erasure of 
		 * type '''Array[T]''' when converting Array of '''T''' to Array of double and vice versa
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98 February 23, 2014
		 * @version 0.99
		 * @see Scala for Machine Learning: Chapter 4 "Unsupervised learning" Clustering / K-means
		 * @note convert the iterative method for the convergence toward minimum total reconstruction 
		 * error into a tail recursion.
		 */
@implicitNotFound(msg = "KMeans type conversion from $T to Double is undefined")
final class KMeans[T <: AnyVal](
		config: KMeansConfig,
		distance: DistanceFunc[T],
		xt: XVSeries[T])
		(implicit m: Manifest[T], num: Numeric[T], f: T => Double) 
	extends ITransform[Array[T]](xt) with Monitor[Double] { 

	check(config)

	  /**
		 * Type of output from K-means
		 */
	type V = Cluster[T]
	val model: Option[KMeansModel[T]] = train
	
	protected val logger = Logger.getLogger("KMeans")
	
	private def train: Option[KMeansModel[T]] = Try {
		val clusters = initialize(xt)
				
				// In the rare case we could not initialize the clusters.
		if( clusters.isEmpty) 
			throw new IllegalStateException("KMeans.|> empty clusters")

		else  {
					// initial assignment of clusters.
			val members = Array.fill(xt.size)(0)
			assignToClusters(xt, clusters, members) 
			var iters = 0
						
					// Implementation of the tail recursion
			@tailrec
			def update(_clusters: KMeansModel[T], xt: XVSeries[T], obs: Array[Int]): KMeansModel[T] = {

					// compute the new clusters by recomputing its centroid if it 
					// is not empty, or adding the observations which distance to the centroid
					// as the highest standard deviation
				
				val newClusters = _clusters.map( c => {
					if( c.size > 0) 
						c.moveCenter(xt) 
					else 
						_clusters.filter( _.size > 0).maxBy( _.stdDev(xt, distance) )
				}) 
							  
					// Update the number of iterations processed then
					// test if no new observation have been transfer to another cluster.
					// If not, the execution exits, otherwise it recurses.
				iters += 1
				if( iters >= config.maxIters || assignToClusters(xt, newClusters, obs) == 0) 
					newClusters
				else 
					update(newClusters, xt, obs)
			}
		
			if( iters >= config.maxIters )
				throw new IllegalStateException("KMeans.|> max iterations exeeded")
					
					// Launch the recursion
			update(clusters, xt, members)
		}
	} match {
		case Success(clusters) => Some(clusters)
		case Failure(e) => none("KMeans.train", e)
	}
	
		/**
		 * Compute the density for all the clusters of the model
		 * @return array of density for the clusters.
		 */
	def density: Option[DblVector] = 
		model.map( _.map( c => 
			c.getMembers.map(xt(_)).map( distance(c.center, _)).sum
		).toVector)


		/**
		 * Override the ITransform operator |> to classify new observation
		 * KMeans++ algorithm with buckets initialization.
		 * @throws MatchError if the input time series is undefined or have no elements
		 * @param xt time series of elements of type T
		 * @return PartialFunction of time series of elements of type T as input to the K-means 
		 * algorithm and a list of cluster (Option) as output
		 */
	override def |> : PartialFunction[Array[T], Try[V]] = {
		case x: Array[T] if( x.length == dimension(xt) && model != None) => 
			Try (model.map(  _.minBy(c => distance(c.center, x)) ).get )
	}
	
	@inline
	private def isLastIter(n: Int): Boolean = n >= config.maxIters
	

		/**
		 * Buckets initialization of centroids and clusters. 
		 */
	private def initialize(xt: XVSeries[T]): KMeansModel[T] = {
			// Compute the statistics related to the time series (1)
		val stats = statistics(xt)   
			
			// Extract the dimension with the highest standard deviation (2)
		val maxSDevDim = Range(0, stats.size).maxBy(stats( _ ).stdDev )
			
			// Rank the observations according to their increasing
			// order of their maximum standard deviation 
		val rankedObs = xt.zipWithIndex
									.map{ case (x, n) => (x(maxSDevDim), n) }
									.sortWith( _._1  < _._1) // Sorted
		 
				// Break down the ranked observations into K buckets
		val halfSegSize = ((rankedObs.size>>1)/config.K).floor.toInt
			
				// Compute the centroids (or center) of each cluster as the mean
				// position of each bucket.
		val centroids = rankedObs.toVector.filter(isContained( _, halfSegSize, rankedObs .size) )
															 .map{ case(x, n) => xt(n) }
														
				// Create a list of K cluster with their associated centroids
		centroids.aggregate(List[Cluster[T]]())((xs, c) => Cluster[T](c) :: xs, _ ::: _)
	}
    

	final private def isContained(t: (T,Int), hSz: Int, dim: Int): Boolean = 
		(t._2 % hSz == 0) && (t._2 %(hSz<<1) != 0)

		/**
		 * The method computes the index of the cluster which is the closest
		 * to each observation, then re-assign them to the nearest cluster.
		 */
	private def assignToClusters(
			xt: XVSeries[T], 
			clusters: KMeansModel[T], 
			members: Array[Int]): Int = {
		
			// Filter to compute the index of the cluster which is 
			// the closest to the data point x
		val nReasigned = xt.view.zipWithIndex.filter{ case (x, n) => { 
			val nearestCluster = getNearestCluster(clusters, x);
			
			// re-assign if the observations does not belong to this nearest cluster
			val reassigned = nearestCluster != members(n) 
			
			// Add the observation to this cluster
			clusters(nearestCluster) += n
			members(n) = nearestCluster
			reassigned
		}}.size
		count("Reasigned", nReasigned)
		nReasigned
	}

		/**
		 * Returns the nearest {@see Cluster} to the given point
		 * @tparam V type of the points to cluster
		 * @param clusters the {@see Cluster}s to search
		 * @param x the point to find the nearest {@see Cluster} for
		 * @return the index of the nearest {@see Cluster} to the given point
		 */
	private def getNearestCluster(clusters: List[Cluster[T]], x: Array[T]): Int = 
		
	  clusters.zipWithIndex./:((Double.MaxValue, 0)){ case (p, (c, n)) => { 
		  	// distance between this observation and the center.
			val measure = distance(c.center, x)  
			if( measure < p._1) (measure, n) else p
		}}._2

}



		/**
		 * Companion object to KMeans define the constructors for the K-means algorithm and
		 * compute the variance of a cluster
		 * @author Patrick Nicolas
		 * @since 0.98.1 February 23, 2014
		 * @version 0.98.3
		 * @see Scala for Machine Learning Chapter 4 "Unsupervised learning" Clustering
		 */
object KMeans {
	import org.scalaml.unsupervised.Distance.euclidean
	import Cluster._
	
	private val MAX_K = 500
	private val MAX_ITERATIONS = 2500

	type KMeansModel[T <: AnyVal] = List[Cluster[T]]
		/**
		 * Default constructor for KMeans
		 * @tparam T type of the elements of observations
		 * @param config configuration for the execution of the KMeans algorithm
		 * @param distance Metric used in computing distance between data points.
		 * @param m Implicit declaration of manifest of type '''T''' to overcome Java erasure of 
		 * type '''Array[T]''' when converting Array of '''T''' to Array of double and vice versa
		 */
	def apply[T <: AnyVal](
			config: KMeansConfig,  
			distance: DistanceFunc[T],
			xt: XVSeries[T])
			(implicit m: Manifest[T], num: Numeric[T], f: T=> Double): KMeans[T] = 
		new KMeans[T](config, distance, xt)

		/**
		 * Constructor for KMeans using the default Euclidean distance metric
		 * @param K Number of clusters
		 * @param config configuration for the execution of the KMeans algorithm
		 * @param m Implicit declaration of manifest of type '''T''' to overcome Java erasure of 
		 * type '''Array[T]''' when converting Array of '''T''' to Array of double and vice versa
		 */
	def apply[T <: AnyVal](
			config: KMeansConfig,
			xt: XVSeries[T])
			(implicit m: Manifest[T], num: Numeric[T], f: T => Double): KMeans[T] = 
		new KMeans[T](config, euclidean, xt)


		/**
		 * Constructor for KMeans using the default Euclidean distance metric 
		 * with a predefined maximum number of iterations for minimizing the
		 * reconstruction error
		 * @tparam T type of the elements of observations
		 * @param K Number of clusters
		 * @param maxIters Maximum number of iterations allowed for the generation of clusters.
		 * @param m Implicit declaration of manifest of type '''T''' to overcome Java erasure of 
		 * type '''Array[T]''' when converting Array of '''T''' to Array of double and vice versa
		 */
	def apply[T <: AnyVal](K: Int, maxIters: Int, distance: DistanceFunc[T], xt: XVSeries[T])
			(implicit m: Manifest[T], num: Numeric[T], f: T=> Double): KMeans[T] = 
		new KMeans[T](KMeansConfig(K, maxIters), distance, xt)

	type U[T] = XVSeries[T]
  type V[T <: AnyVal] = List[Cluster[T]]
	
		/**
		 * Computes the standard deviation of the distance of the data point to the
		 * center of their respective cluster
		 * @tparam T type of the elements of observations
		 * @param clusters List of clusters
		 * @param xt Input time series
		 * @param distance Distance metric used in evaluating distances between data points and 
		 * centroids.
		 * @throws IllegalArgumentException if one of the parameters is undefined
		 * @return list of standard deviation values for all the clusters.
		 */
	@throws(classOf[IllegalArgumentException])
	def stdDev[T <: AnyVal](clusters: V[T], xt: U[T], distance: DistanceFunc[T]): DblVector = {
		require( !clusters.isEmpty, 
				"KMeans.stdDev Cannot compute the variance of undefined clusters")
		require( !xt.isEmpty, 
				"KMeans.stdDev  Cannot compute the variance of clusters for undefined input data")
				// Compute the standard deviation of the distance within each cluster
		clusters.map( _.stdDev(xt, distance)).toVector
	}
	
   
	
	private def check(config: KMeansConfig): Unit = {
		require(config.K > 0 && config.K < MAX_K, 
				s"KMeans.check found K= ${config.K} required > 0 and < $MAX_K")
		require( config.maxIters > 1 && config.maxIters < MAX_ITERATIONS, 
				s"KMean Found ${config.maxIters} required > 1 and < MAX_ITERATIONS")
	}
}


// ----------------------------  EOF -------------------------------------------
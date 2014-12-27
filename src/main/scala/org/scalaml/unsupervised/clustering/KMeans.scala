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
package org.scalaml.unsupervised.clustering

import scala.annotation.implicitNotFound

import org.apache.log4j.Logger

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.Design.PipeOperator
import org.scalaml.unsupervised.Distance
import org.scalaml.util.DisplayUtils
import ScalaMl._

		/**
		 * <p>Class that implements the KMeans++ algorithm for which the centroids
		 * are initialized at mid point of K segments of data points after the data points
		 * are ordered by their variance.<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;
		 * ,Arial,Helvetica,sans-serif;">
		 *  Minimize the reconstruction error SUM all clusters [SUM d(x(i), m(k)] x(i) belonging to Cluster k with center m(k)</span></pre></p>
		 * @constructor Initiate a K-means algorithm with a predefined number of cluster, maximum 
		 * number of iterations and a distance metric. 
		 * @throws IllegalArgumentException if the number of clusters or the maximum number of 
		 * iterations is out of range or if the distance metric is undefined.
		 * @throws implicitNotFoundException if the ordering instance is not implicitly defined.
		 * @param K Number of clusters
		 * @param maxIters Maximum number of iterations allowed for the generation of clusters.
		 * @param distance Metric used in computing distance between data points.
		 * @param m Implicit declaration of manifest of type <b>T</b> to overcome Java erasure of 
		 * type <b>Array[T]</b> when converting Array of <b>T</b> to Array of double and vice versa
		 * 
		 * @author Patrick Nicolas
		 * @since February 23, 2014
		 * @note Scala for Machine Learning: Chapter 4 Unsupervised learning / Clustering / K-means
		 */
@implicitNotFound("Ordering not implicitly defined for K-means")
final class KMeans[T <% Double](
		K: Int, 
		maxIters: Int, 
		distance: (DblVector, Array[T]) => Double)
		(implicit order: Ordering[T], m: Manifest[T]) 
			extends PipeOperator[XTSeries[Array[T]], List[Cluster[T]]] { 

	import XTSeries._, KMeans._
	check(K, maxIters)
	
	private val logger = Logger.getLogger("KMeans")
    	
		/**
		 * <p>Overload data transformation/Pipe operator to implement
		 * KMeans++ algorithm with buckets initialization.</p>
		 * @throws MatchError if the input time series is undefined or have no elements
		 * @param xt time series of elements of type T
		 * @return PartialFunction of time series of elements of type T as input to the K-means 
		 * algorithm and a list of cluster as output
		 */
	override def |> : PartialFunction[XTSeries[Array[T]], List[Cluster[T]]] = {
		case xt: XTSeries[Array[T]] if( !xt.isEmpty && dimension(xt) > 0) => {
			val clusters = initialize(xt)  // 1 
				
			// In the rare case we could not initialize the clusters.
			if( clusters.isEmpty) List.empty
			else  {
				// initial assignment of clusters.
				val membership = Array.fill(xt.size)(0)
				val reassigned = assignToClusters(xt, clusters, membership) //2
				var newClusters: List[Cluster[T]] = List.empty			   		 
		
				// Reassign iteratively the data points to existing clusters. 
				// The clusters are re-created at each iterations. The iteration
				// stops when there is no more data point to re-assigned
				Range(0,  maxIters).find( _ => {
					
					// Walk through each cluster. If a cluster is empty
					// the data points are redistributed across all the clusters
					// using the standard deviation, otherwise the observations are moved 
					// within the clusters
					newClusters = clusters.map( c => {
						if( c.size > 0) 
							c.moveCenter(xt) 
						else 
							clusters.filter( _.size > 0).maxBy( _.stdDev(xt, distance) )
					}) 

					// Re-assign the observations to the clusters
					assignToClusters(xt, newClusters, membership) > 0
				}).map(_ => newClusters).getOrElse(List.empty)
				/*
				match {    // Failed if the maximum number of iterations is reached.
					case Some(index) => newClusters
					case None => List.empty
				} 
				* 
				*/
			}
		}
	}

		/**
		 * Buckets initialization of centroids and clusters. 
		 */
	private def initialize(xt: XTSeries[Array[T]]): List[Cluster[T]] = {

			// Compute the statistics related to the time series (1)
		val stats = statistics(xt)   
			// Extract the dimension with the highest standard deviation (2)
		val maxSDevDim = Range(0, stats.size).maxBy(stats( _ ).stdDev )
		
			// Rank the observations according to their increasing
			// order of their maximum standard deviation 
		val rankedObs = xt.zipWithIndex
							.map( x=> (x._1(maxSDevDim), x._2) )
							.sortWith( _._1  < _._1).toArray // Sorted
	 
			// Break down the ranked observations into K buckets
		val halfSegSize = ((rankedObs.size>>1)/K).floor.toInt
		
			// Compute the centroids (or center) of each cluster as the mean
			// position of each bucket.
		val centroids = rankedObs.filter(isContained( _, halfSegSize, rankedObs .size) )
								.map( x => xt( x._2))
								
			// Create a list of K cluster with their associated centroids=
		Range(0, K).foldLeft(List[Cluster[T]]())((xs, i) 
					=> Cluster[T](centroids(i)) :: xs)
	}
    

	final private def isContained(t: (T,Int), hSz: Int, dim: Int): Boolean = 
		(t._2 % hSz == 0) && (t._2 %(hSz<<1) != 0)

		/**
		 * The method computes the index of the cluster which is the closest
		 * to each observation, then re-assign them to the nearest cluste.
		 */
	private def assignToClusters(
			xt: XTSeries[Array[T]], 
			clusters: List[Cluster[T]], 
			membership: Array[Int]): Int =  {
		
			// Filter to compute the index of the cluster which is 
			// the closest to the data point x
		xt.toArray.zipWithIndex
					.filter( x => { 
			val nearestCluster = getNearestCluster(clusters, x._1);
			
			// re-assign if the observations does not belong to this nearest cluster
			val reassigned = nearestCluster != membership(x._2) 
			
			// Add the observation to this cluster
			clusters(nearestCluster) += x._2
			membership(x._2) = nearestCluster
			reassigned
		}).size
	}

		/**
		 * Returns the nearest {@link Cluster} to the given point
		 * @param <T> type of the points to cluster
		 * @param clusters the {@link Cluster}s to search
		 * @param point the point to find the nearest {@link Cluster} for
		 * @return the index of the nearest {@link Cluster} to the given point
		 */
	private[this] def getNearestCluster(clusters: List[Cluster[T]], x: Array[T]): Int = {
		clusters.zipWithIndex.foldLeft((Double.MaxValue, 0))((p, c) => { 
			val measure = distance(c._1.center, x)  // distance between this obs and the center.
			if( measure < p._1) 
				(measure, c._2) 
			else p
		})._2
	}   
}



		/**
		 * Companion object to KMeans define the constructors for the K-means algorithm and
		 * compute the variance of a cluster
		 * @author Patrick Nicolas
		 * @since February 23, 2014
		 * @note Scala for Machine Learning: Chapter 4 Unsupervised learning/Clustering
		 */
object KMeans {
	import org.scalaml.unsupervised.Distance.euclidean
   
	private val MAX_K = 500
	private val MAX_ITERATIONS = 5000
 
		/**
		 * Default constructor for KMeans
		 * @param K Number of clusters
		 * @param maxIters Maximum number of iterations allowed for the generation of clusters.
		 * @param distance Metric used in computing distance between data points.
		 * @param m Implicit declaration of manifest of type <b>T</b> to overcome Java erasure of 
		 * type <b>Array[T]</b> when converting Array of <b>T</b> to Array of double and vice versa
		 */
	def apply[T <% Double](
			K: Int, 
			maxIters: Int, 
			distance: (DblVector, Array[T]) => Double)
			(implicit order: Ordering[T], m: Manifest[T]): KMeans[T] = new KMeans[T](K, maxIters, distance)

		/**
		 * Constructor for KMeans using the default Euclidean distance metric
		 * @param K Number of clusters
		 * @param maxIters Maximum number of iterations allowed for the generation of clusters.
		 * @param m Implicit declaration of manifest of type <b>T</b> to overcome Java erasure of 
		 * type <b>Array[T]</b> when converting Array of <b>T</b> to Array of double and vice versa
		 */
	def apply[T <% Double](
			K: Int, 
			maxIters: Int)
			(implicit order: Ordering[T], m: Manifest[T]): KMeans[T] = 
		new KMeans[T](K, maxIters, euclidean)


		/**
		 * Constructor for KMeans using the default Euclidean distance metric 
		 * with a predefined maximum number of iterations for minimizing the
		 * reconstruction error
		 * @param K Number of clusters
		 */
	def apply[T <% Double](K: Int)(implicit order: Ordering[T], m: Manifest[T]): KMeans[T] = 
		new KMeans[T](K, MAX_ITERATIONS, euclidean)

		/**
		 * Computes the standard deviation of the distance of the data point to the
		 * center of their respective cluster
		 * @param c List of clusters
		 * @param xt Input time series
		 * @param distance Distance metric used in evaluating distances between data points and 
		 * centroids.
		 * @throws IllegalArgumentException if one of the parameters is undefined
		 * @return list of standard deviation values for all the clusters.
		 */
	def stdDev[T](
			c: List[Cluster[T]], 
			xt: XTSeries[Array[T]], 
			distance: (DblVector, Array[T]) => Double): List[Double] = {
		require( !c.isEmpty, 
				"KMeans.stdDev Cannot compute the variance of undefined clusters")
		require( !xt.isEmpty, 
				"KMeans.stdDev  Cannot compute the variance of clusters for undefined input datat")
				// Compute the standard deviation of the distance within each cluster
		c.map( _.stdDev(xt, distance))
	}
   
	
	private def check(K: Int,  maxIters: Int): Unit = {
		require(K > 0 && K < MAX_K, 
				s"KMeans.check Number of clusters for Kmeans $K is out of range")
		require( maxIters > 1 && maxIters < MAX_ITERATIONS, 
				s"KMeans.check Maximum number of iterations for Kmeans $maxIters is out of range")
	}
}


// ----------------------------  EOF -------------------------------------------
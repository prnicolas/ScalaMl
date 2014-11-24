/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.unsupervised.clustering

import org.scalaml.core.{XTSeries, types}
import org.scalaml.core.design.PipeOperator
import scala.Array.canBuildFrom
import scala.annotation.implicitNotFound
import org.scalaml.unsupervised.Distance
import types.ScalaMl._
import KMeans._
import org.apache.log4j.Logger
import org.scalaml.util.Display

		/**
		 * <p>Class that implements the KMeans++ algorithm for which the centroids
		 * are initialized at mid point of K segments of data points after the data points
		 * are ordered by their variance.<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
		 * <b>K</b>         Number of clusters
		 * <b>maxIters</b>  Maximum number of iterations allowed for the generation of clusters.
		 * <b>distance</b>  Metric used in computing distance between data points.
		 * <b>m</b>         Implicit declaration of manifest of type <b>T</b> to overcome Java erasure of type <b>Array[T]</b> when converting Array of <b>T</b> to Array of double and vice vers
		 * </span></pre></p>
		 * @constructor Initiate a K-means algorithm with a predefined number of cluster, maximum number of iterations and a distance metric. 
		 * @throws IllegalArgumentException if the number of clusters or the maximum number of 
		 * iterations is out of range or if the distance metric is undefined.
		 * @throws implicitNotFoundException if the ordering instance is not implicitly defined.
		 * 
		 * @author Patrick Nicolas
		 * @since February 23, 2014
		 * @note Scala for Machine Learning: Chapter 4 Unsupervised learning/Clustering
		 */
@implicitNotFound("Ordering not implicitly defined for K-means")
final class KMeans[T <% Double](K: Int, maxIters: Int, distance: (DblVector, Array[T]) => Double)(implicit order: Ordering[T], m: Manifest[T]) 
				extends PipeOperator[XTSeries[Array[T]], List[Cluster[T]]] { 

	import XTSeries._, KMeans._
	check(K, maxIters, distance)
	
	private val logger = Logger.getLogger("KMeans")
    	
		/**
		 * <p>Overload data transformation/Pipe operator to implement
		 * KMeans++ algorithm with buckets initialization.</p>
		 * @throws MatchError if the input time series is undefined or have no elements
		 * @param xt time series of elements of type T
		 * @return PartialFunction of time series of elements of type T as input to the K-means algorithm and a list of cluster as output
		 */
	override def |> : PartialFunction[XTSeries[Array[T]], List[Cluster[T]]] = {
		case xt: XTSeries[Array[T]] if(xt != null && xt.size > 1 && xt(0).size > 0) => {
			val clusters = initialize(xt)  // 1 
				
			// In the rare case we could not initialize the clusters.
			if( clusters.isEmpty) List.empty
			else  {
				// initial assignment of clusters.
				val membership = Array.fill(xt.size)(0)
				val reassigned = assignToClusters(xt, clusters, membership) //2
				var newClusters: List[Cluster[T]] = List.empty			   		 
		
				// Reassign iteratively data points to 
				// existing clusters. The clusters are re-created at each iterations
				Range(0,  maxIters).find( _ => {
					newClusters = clusters.map( c => {
						if( c.size > 0) 
							c.moveCenter(xt) 
						else 
							clusters.filter( _.size > 0).maxBy( _.stdDev(xt, distance) )
					}) 

					assignToClusters(xt, newClusters, membership) > 0
				}) 
				match {    // Failed if the maximum number of iterations is reached.
					case Some(index) => newClusters
					case None => List.empty
				} 
			}
		}
	}

		/**
		 * Buckets initialization of centroids and clusters. 
		 */
	private def initialize(xt: XTSeries[Array[T]]): List[Cluster[T]] = {
        
		val stats = statistics(xt)   // step 1
		val maxSDevDim = Range(0, stats.size).maxBy(stats( _ ).stdDev )
		val rankedObs = xt.zipWithIndex
							.map( x=> (x._1(maxSDevDim), x._2) )
							.sortWith( _._1  < _._1).toArray // 3
	    
		val halfSegSize = ((rankedObs.size>>1)/K).floor.toInt

		val centroids = rankedObs.filter(isContained( _, halfSegSize, rankedObs .size) )
								.map( x => xt( x._2))
		Range(0, K).foldLeft(List[Cluster[T]]())((xs, i) => Cluster[T](centroids(i)) :: xs)
	}
    

	final private def isContained(t: (T,Int), hSz: Int, dim: Int): Boolean = (t._2 % hSz == 0) && (t._2 %(hSz<<1) != 0)
			
	private def assignToClusters(xt: XTSeries[Array[T]], clusters: List[Cluster[T]], membership: Array[Int]): Int =  {
		xt.toArray.zipWithIndex.filter( x => {
			val nearestCluster = getNearestCluster(clusters, x._1);
			val reassigned = nearestCluster != membership(x._2) 
			
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
			val measure = distance(c._1.center, x)
			if( measure < p._1) 
				(measure, c._2) 
			else p
		})._2
	}   
}



		/**
		 * Companion object to KMeans define the constructors for the K-means algorithm and
		 * compute the variance of a cluster
		 */
object KMeans {
	import org.scalaml.unsupervised.Distance.euclidean
   
	final val MAX_K = 500
	final val MAX_ITERATIONS = 5000
   
	def apply[T <% Double](K: Int, maxIters: Int, distance: (DblVector, Array[T]) => Double)(implicit order: Ordering[T], m: Manifest[T]): KMeans[T] = 
		new KMeans[T](K, maxIters, distance)
   
	def apply[T <% Double](K: Int, maxIters: Int)(implicit order: Ordering[T], m: Manifest[T]): KMeans[T] = 
		new KMeans[T](K, maxIters, euclidean)

	def apply[T <% Double](K: Int)(implicit order: Ordering[T], m: Manifest[T]): KMeans[T] = 
		new KMeans[T](K, MAX_ITERATIONS, euclidean)
   
	def stdDev[T](c: List[Cluster[T]], xt: XTSeries[Array[T]], distance: (DblVector, Array[T]) => Double): List[Double] =  {
		require(c != null && c.size > 0, "KMeans.stdDev Cannot compute the variance of undefined clusters")
		require(xt != null && xt.size > 0, "KMeans.stdDev  Cannot compute the variance of clusters for undefined input datat")
  	 
		c.map( _.stdDev(xt, distance))
	}
   
	
	private def check[T <% Double](K: Int, maxIters: Int, distance: (DblVector, Array[T]) => Double): Unit = {
		require(K > 0 && K < MAX_K, s"KMeans.check Number of clusters for Kmeans $K is out of range")
		require( maxIters > 1 && maxIters < MAX_ITERATIONS, s"KMeans.check Maximum number of iterations for Kmeans $maxIters is out of range")
		require( distance != null, "KMeans.check Distance for K-means is undefined")
	}
}


// ----------------------------  EOF -------------------------------------------
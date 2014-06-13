/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.unsupervised.clustering

import org.scalaml.core.{XTSeries, Types}
import org.scalaml.workflow.PipeOperator
import scala.Array.canBuildFrom
import scala.annotation.implicitNotFound
import org.scalaml.unsupervised.Distance


		/**
		 * <p>Class that implements the KMeans++ algorithm for which the centroids
		 * are initialized at mid point of K segments of data points after the data points
		 * are ordered by their variance.</p>
		 * @param K number of clusters
		 * @param maxIters maximum number of iterations allowed for the generation of clusters (default: 5000)
		 * @param distance metric used in computing distance between data points.
		 * @param order implicit ordering used in the initialization of of centroids
		 * @param m implicit declaration of manifest of type T to overcome Java erasure of type when
		 * converting Array of T to Array of double and vice versa
		 * 
		 * @exception IllegalArgumentException if the number of clusters or the maximum number of 
		 * iterations is out of range or if the distance metric is undefined.
		 * @exception implicitNotFoundExceptin if the ordering instance is not implicitly defined.
		 * 
		 * @author Patrick Nicolas
		 * @date February 23, 2014
		 * @project Scala for Machine Learning
		 */
import Types.ScalaMl._
import KMeans._
@implicitNotFound("Ordering not implicitly defined for K-means")
final class KMeans[T <% Double](val K: Int, val maxIters: Int, val distance: (DblVector, Array[T]) => Double)(implicit order: Ordering[T], m: Manifest[T]) 
                                   extends PipeOperator[XTSeries[Array[T]], List[Cluster[T]]] { 
    import XTSeries._, Types.ScalaMl._
    
    require(K > 0 && K < MAX_K, "Number of clusters for Kmeans " + K + " is out of range")
    require( maxIters > 1 && maxIters < MAX_ITERATIONS, "Maximum number of iterations for Kmeans " + maxIters + " is out of range")
    require( distance != null, "Distance for K-means is undefined")
    
    	/**
    	 * <p>Overload data transformation/Pipe operator to implement
    	 * KMeans++ algorithm with buckets initialization.</p>
    	 */
    def |> (xt: XTSeries[Array[T]]): Option[List[Cluster[T]]] = {
        require(xt != null && xt.size > 0, "Cannot apply K-means to undefined dataset")	
    	
	   val clusters = initialize(xt)  // 1
			   // In the rare case we could not initialize the clusters.
	   if( clusters.isEmpty)  None
	   else  {
	  	 	// initial assignment of clusters.
		   val membership = Array.fill(xt.size)(0)
		   val reassigned = assignToClusters(xt, clusters, membership) //2
		   var newClusters: List[Cluster[T]] = null
		   		 
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
		   }) match {    // Failed if the maximum number of iterations is reached.
		  	  case Some(index) => Some(newClusters)
		  	  case None => None
		   } 
	   }
	}
    

    	/**
    	 * Buckets initialization of centroids and clusters. 
    	 */
    private def initialize(xt: XTSeries[Array[T]]): List[Cluster[T]] = {
        import Types.ScalaMl._
        
	    val stats = statistics(xt)   // step 1
		val maxSDevIdx = Range(0, stats.size).maxBy (stats( _ ).stdDev )
	    val maxStdDev = xt.arr.zipWithIndex.map( x=> (x._1(maxSDevIdx), x._2) ).sortWith( _._1  < _._1) // 3
	    val halfSegSize = (maxStdDev.size*0.5/K).floor.toInt

	    val centroids = maxStdDev.filter(screen( _, halfSegSize, maxStdDev.size) ).map( x => xt( x._2))
		Range(0, K).foldLeft(List[Cluster[T]]())((xs, i) => Cluster[T](centroids(i)) :: xs)
	}
    
    private def screen(t: (T,Int), hSz: Int, dim: Int): Boolean = 
       ((t._2 % hSz == 0 && t._2 %(hSz<<1) != 0) || t._2 == dim -1)
  

	private def assignToClusters(xt: XTSeries[Array[T]], clusters: List[Cluster[T]], membership: Array[Int]): Int =  {
	   xt.arr.zipWithIndex.filter( x => {
	     val nearestCluster = getNearestCluster(clusters, x._1);
	     val reassigned = nearestCluster != membership(x._2) 
	     clusters(nearestCluster) += x._2
	     membership(x._2) = nearestCluster
	     reassigned
	   }).size
	}
     

    /**
     * Returns the nearest {@link Cluster} to the given point
     *
     * @param <T> type of the points to cluster
     * @param clusters the {@link Cluster}s to search
     * @param point the point to find the nearest {@link Cluster} for
     * @return the index of the nearest {@link Cluster} to the given point
     */
    private[this] def getNearestCluster(clusters: List[Cluster[T]], x: Array[T]): Int = {
      clusters.zipWithIndex.foldLeft((Double.MaxValue, 0))((p, c) => { 
         val measure = distance(c._1.center, x)
         if( measure < p._1) (measure, c._2) else p
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
   
   def apply[T <% Double](K: Int, maxIters: Int, distance: (DblVector, Array[T]) => Double)(implicit order: Ordering[T], m: Manifest[T]): KMeans[T] = new KMeans[T](K, maxIters, distance)
   def apply[T <% Double](K: Int, maxIters: Int)(implicit order: Ordering[T], m: Manifest[T]): KMeans[T] = new KMeans[T](K, maxIters, euclidean)

   def apply[T <% Double](K: Int)(implicit order: Ordering[T], m: Manifest[T]): KMeans[T] = new KMeans[T](K, MAX_ITERATIONS, euclidean)
   
   def stdDev[T](c: List[Cluster[T]], xt: XTSeries[Array[T]], distance: (DblVector, Array[T]) => Double): List[Double] =  {
  	  require(c != null && c.size > 0, "Cannot compute the variance of undefined clusters")
  	  require(xt != null && xt.size > 0, "Cannot compute the variance of clusters for undefined input datat")
  	 
  	  c.map( _.stdDev(xt, distance))
   }
}


// ----------------------------  EOF -------------------------------------------
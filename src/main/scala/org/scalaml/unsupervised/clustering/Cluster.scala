/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.unsupervised.clustering

import org.scalaml.core.{XTSeries, Types}
import org.scalaml.stats.Stats
import scala.collection.mutable.ListBuffer
import org.scalaml.unsupervised.Distance.euclidean



		/**
		 * <p>Class that define a cluster used in the KMeans++ clustering algorithm.
		 * A cluster is defined by its center (scalar or vector) and its members (data point)
		 * it contains. The membership of data points to this cluster is done through their index.
		 * It is assumed that each data point has a unique index and therefore a cluster will never
		 * contains two data points with the same index.</p>
		 * @param center data point acting as the center of the cluster
		 * @throws IllegalArgumenException if the center is undefined (null)
		 * 
		 * @author Patrick Nicolas
		 * @since February 22, 2014
		 * @note Scala for Machine Learning
		 */
import Types.ScalaMl._
import XTSeries._
class Cluster[T <% Double](val center: DblVector) {
   require(center != null && center.size > 0, "Cannot create a cluster with undefined centers")
   
   private val members = new ListBuffer[Int]
   
	/**
	 * <p>Overloaded operator that add a new data point by its index in the membership.
	 * There is no validation whether this data points is already a member of this cluster
	 * or any other clusters.</p>
	 */
   def += (n:Int): Unit = members.append(n)
   final def size: Int = members.size
   
   	/**
   	 * <p>Recompute the coordinates for the center of this cluster.</p>
   	 * @param xt Time series of observations used in the re-computation of the center
   	 * @throws IllegalArgumentException if the time series argument is undefined
   	 * @return a new cluster with the recomputed center.
   	 */
   final def moveCenter(xt: XTSeries[Array[T]]): Cluster[T] = {  
  	   require(xt != null && xt.size > 0, "Cannot migrate undefined times series values within the cluster" )
  	   
  	   val centroid = Array.fill(dimension(xt))(0.0)
  	   members.map(xt( _ )).foreach( Op( _, centroid, (x:T, y:Double)=>x+y))
  	   Cluster[T](/ (centroid, members.size))
   }
   
   
   /**
    * <p>Compute the standard deviation of the members of this cluster from its center.</p>
    * @param xt time series used in the computation of the center
    * @param distance metric used to measure the distance between the center and any of the member of the cluster.
    * @throws IllegalArgumentException if the time series argument is undefined
    * @return standard deviation of all the members from the center of the cluster.
    */
   final def stdDev(xt: XTSeries[Array[T]], distance: (DblVector, Array[T])=> Double ): Double =  {
  	  require(xt != null && xt.size > 0, "Cannot compute the standard deviation within this cluster for undefined times series")
      require( distance != null, "Cannot compute the standard deviation within a cluster with undefined distance")
      
  	  Stats[Double](members.map( xt( _)).map( distance(center, _)).toArray).stdDev
   }  
   
   final def getMembers: List[Int] = members.toList
   
   override def toString: String = 
     members.foldLeft(new StringBuffer)((b, n) => b.append(n).append(",")).toString
}



object Cluster {
   def apply[T <% Double](center: DblVector): Cluster[T] = new Cluster[T](center)
}



// ----------------------------  EOF -------------------------------------------
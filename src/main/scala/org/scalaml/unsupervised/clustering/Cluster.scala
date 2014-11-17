/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 */
package org.scalaml.unsupervised.clustering

import org.scalaml.core.{XTSeries, types}
import org.scalaml.stats.Stats
import scala.collection.mutable.ListBuffer
import org.scalaml.unsupervised.Distance.euclidean
import types.ScalaMl._
import XTSeries._


		/**
		 * <p>Class that define a cluster used in the KMeans++ clustering algorithm.
		 * A cluster is defined by its center (scalar or vector) and its members (data point)
		 * it contains. The membership of data points to this cluster is done through their index.
		 * It is assumed that each data point has a unique index and therefore a cluster will never
		 * contains two data points with the same index.<br>
		 * <b>center</b> Initial centroid for this cluster</p>
		 * @constructor Instantiate a cluster with an initial centroid. 
		 * @throws IllegalArgumenException if the center is undefined (null)
		 * 
		 * @author Patrick Nicolas
		 * @since February 22, 2014
		 * @note Scala for Machine Learning Chapter 4 Unsupervised learning / K-means clustering
		 */
class Cluster[T <% Double](val center: DblVector) {
	require(center != null && center.size > 0, "Cluster Cannot create a cluster with undefined centers")
   
	private val members = new ListBuffer[Int]
   
		/**
		 * <p>Overloaded operator += to add a new data point by its index in the membership.
		 * There is no validation whether this data points is already a member of this cluster
		 * or any other clusters.</p>
		 * @param n index of the new data point
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
		require(xt != null && xt.size > 0, "Cluster.moveCenter Cannot migrate undefined times series values within the cluster" )
  	   
		val sums = members.map(xt(_).map(_.toDouble)).toList.transpose.map( _.sum)
		Cluster[T](sums.map( _ / members.size).toArray)
	}
   
		/**
		 * <p>Compute the standard deviation of the members of this cluster from its center.</p>
		 * @param xt time series used in the computation of the center
		 * @param distance metric used to measure the distance between the center and any of the member of the cluster.
		 * @throws IllegalArgumentException if the time series argument is undefined
		 * @return standard deviation of all the members from the center of the cluster.
		 */
	final def stdDev(xt: XTSeries[Array[T]], distance: (DblVector, Array[T])=> Double ): Double =  {
		require(xt != null && xt.size > 0, 
			"Cluster.stdDev Cannot compute the standard deviation within this cluster for undefined times series")
		require(distance != null, 
			"Cluster.stdDev Cannot compute the standard deviation within a cluster with undefined distance")
		require(members.size > 0, "Cluster.stdDev this cluster has no member")

		Stats[Double](members.map( xt( _)).map( distance(center, _)).toArray).stdDev
	}
   
		/**
		 * <p>Returns the list of index of the data points (members) that belong to this cluster
		 * @return list of index of the members of this clusters
		 */
	final def getMembers: List[Int] = members.toList
   
	override def toString: String = {
		val membersToString = members.foldLeft(new StringBuffer)((b, n) => b.append(s"$n,")).toString
		val centerToString = center.foldLeft(new StringBuffer)((b, x) => b.append(s"$x,")).toString
		s"${centerToString}\n${membersToString}"
	}
}


		/**
		 * * Companion object for the Cluster class. The singleton is used
		 *  to define the constructor for this cluster.
		 */
object Cluster {
	def apply[T <% Double](center: DblVector): Cluster[T] = new Cluster[T](center)
	def apply[T <% Double]: Cluster[T] = new Cluster[T](Array.empty)
}



// ----------------------------  EOF -------------------------------------------
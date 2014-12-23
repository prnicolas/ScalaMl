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

import scala.collection.mutable.ListBuffer

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.stats.Stats
import org.scalaml.unsupervised.Distance.euclidean
import org.scalaml.util.FormatUtils
import ScalaMl._, XTSeries._


		/**
		 * <p>Class that define a cluster used in the KMeans++ clustering algorithm.
		 * A cluster is defined by its center (scalar or vector) and its members (data point)
		 * it contains. The membership of data points to this cluster is done through their index.
		 * It is assumed that each data point has a unique index and therefore a cluster will never
		 * contains two data points with the same index.
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;
		 * ,Arial,Helvetica,sans-serif;">
		 * Minimize the reconstruction error SUM all clusters [SUM d(x(i), m(k)] x(i) belonging to Cluster k with center m(k)</span></pre></p>
		 * @constructor Instantiate a cluster with an initial centroid. 
		 * @throws IllegalArgumenException if the center is undefined (null)
		 * @param Initial centroid for this cluster
		 * @author Patrick Nicolas
		 * @since February 22, 2014
		 * @note Scala for Machine Learning Chapter 4 Unsupervised learning / K-means clustering
		 */
class Cluster[T <% Double](val center: DblVector) {
	require( !center.isEmpty, "Cluster Cannot create a cluster with undefined centers")
	
		// List of observations 'members' belonging to this cluster
	private[this] val members = new ListBuffer[Int]
   
		/**
		 * <p>Overloaded operator += to add a new data point by its index in the membership.
		 * There is no validation whether this data points is already a member of this cluster
		 * or any other clusters.</p>
		 * @param n index of the new data point
		 */
	def += (n:Int): Unit = members.append(n)
   
		/**
		 * Return the number of data points in this cluster
		 * @return number of members in the clsuter
		 */
	final def size: Int = members.size
   
		/**
		 * <p>Recompute the coordinates for the center of this cluster.</p>
		 * @param xt Time series of observations used in the re-computation of the center
		 * @throws IllegalArgumentException if the time series argument is undefined
		 * @return a new cluster with the recomputed center.
		 */
	final def moveCenter(xt: XTSeries[Array[T]]): Cluster[T] = {  
		require( !xt.isEmpty, "Cluster.moveCenter Cannot relocate time series datapoint" )
  	 
			// Compute the sum of the value of each dimension of the time series ... 
			// The matrix observations x features has to be transposed in order to 
			// compute the sum of observations for each featuer
		val sums = members.map(xt(_)
							.map(_.toDouble))
							.toList
							.transpose
							.map( _.sum)
							
			// then average it by the number  of data points in the cluster
		Cluster[T](sums.map( _ / members.size).toArray)
	}
   
		/**
		 * <p>Compute the standard deviation of the members of this cluster from its center.</p>
		 * @param xt time series used in the computation of the center
		 * @param distance metric used to measure the distance between the center and any of the 
		 * member of the cluster.
		 * @throws IllegalArgumentException if the time series argument is undefined
		 * @return standard deviation of all the members from the center of the cluster.
		 */
	final def stdDev(xt: XTSeries[Array[T]], distance: (DblVector, Array[T])=> Double ): Double = {
		require( !xt.isEmpty, 
			"Cluster.stdDev Cannot compute the standard deviation wih  undefined times series")
		assert(members.size > 0, "Cluster.stdDev this cluster has no member")
		
			// Extract the vector of the distance between each data
			// point and the current center of the cluster.
		val ts: DblVector  = members.map( xt( _))				// convert a vector
									.map( distance(center, _))	// compute the distance between point and center
									.toArray
									
			// Compute the standard deviation of the distance between the
			// data points and the center of the cluster using the Stats class
		Stats[Double](ts).stdDev
	}
   
		/**
		 * <p>Returns the list of index of the data points (members) that belong to this cluster
		 * @return list of index of the members of this clusters
		 */
	final def getMembers: List[Int] = members.toList

			/**
			 * <p>Textual representation of a cluster for debugging purpose.</p>
			 * @return String representation of this clsuter
			 */
	override def toString: String = {
			// First collect list of observations members to this cluster
		val membersList = members.foldLeft(new StringBuffer)((b, n) => b.append(s"\t   $n")).toString
		
			// Then collect the value of centroids..
		val centerString = center.foldLeft(new StringBuffer)((b, x) => {
			val x_str = FormatUtils.format(x, "", FormatUtils.ShortFormat)
		  b.append(s"$x_str ")
		}).toString
		s"Cluster definition\nCentroids: ${centerString}\nMembership: ${membersList}"
	}
}


		/**
		 * Companion object for the Cluster class. The singleton is used
		 * to define the constructor for this cluster.
		 * @author Patrick Nicolas
		 * @since February 22, 2014
		 * @note Scala for Machine Learning Chapter 4 Unsupervised learning / K-means clustering
		 */
object Cluster {
		/**
		 * Default constructor for a cluster
		 * @param Initial centroid for this cluster
		 */
	def apply[T <% Double](center: DblVector): Cluster[T] = new Cluster[T](center)
	
		/**
		 * Constructor for a Cluster with undefined center.
		 */
	def apply[T <% Double]: Cluster[T] = new Cluster[T](Array.empty)
}

// ----------------------------  EOF -------------------------------------------
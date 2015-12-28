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
package org.scalaml.unsupervised.clustering

import scala.collection.mutable.ListBuffer

import org.scalaml.core.Types.{ScalaMl, emptyString}
import org.scalaml.stats.{XTSeries, Transpose, Stats}

import org.scalaml.unsupervised.Distance.euclidean
import org.scalaml.util.FormatUtils._
import ScalaMl._, XTSeries._, Cluster._, Transpose._


		/**
		 * Class that define a cluster used in the KMeans++ clustering algorithm.
		 * A cluster is defined by its center (scalar or vector) and its members (data point)
		 * it contains. The membership of data points to this cluster is done through their index.
		 * It is assumed that each data point has a unique index and therefore a cluster will never
		 * contains two data points with the same index.
		 * {{{
		 *   Minimize the reconstruction error SUM all clusters [SUM d(x(i), m(k)] x(i) 
		 *   belonging to Cluster k with center m(k)
		 * }}}
		 * @tparam T type of the 
		 * @constructor Instantiate a cluster with an initial centroid. 
		 * @throws IllegalArgumentException if the center is undefined (null)
		 * @param center initial centroid for this cluster
		 * @author Patrick Nicolas
		 * @since 0.98 February 22, 2014
		 * @version 0.98.2
		 * @see Scala for Machine Learning Chapter 4 "Unsupervised learning" K-means clustering
		 */
@throws(classOf[IllegalArgumentException])
private[scalaml] final class Cluster[T <: AnyVal](val center: DblArray)(implicit f: T => Double) {
	require( center.length > 0, "Cluster Cannot create a cluster with undefined centers")

	
		// List of observations 'members' belonging to this cluster
	private[this] val members = new ListBuffer[Int]
   
		/**
		 * Overloaded operator += to add a new data point by its index in the membership.
		 * There is no validation whether this data points is already a member of this cluster
		 * or any other clusters.
		 * @param n index of the new data point
		 */
	def += (n: Int): Unit = members.append(n)
   
		/**
		 * Return the number of data points in this cluster
		 * @return number of members in the cluster
		 */
	final def size: Int = members.size
	
	final def distanceToCentroid(x: DblArray, distance: DistanceFunc[Double]): Double = distance(center, x)
	
		/**
		 * Recompute the coordinates for the center of this cluster.
		 * @param xt Time series of observations used in the re-computation of the center
		 * @throws IllegalArgumentException if the time series argument is undefined
		 * @return a new cluster with the recomputed center.
		 */
	@throws(classOf[IllegalArgumentException])
	@throws(classOf[IllegalStateException])
	final def moveCenter(
			xt: XVSeries[T])
		(implicit m: Manifest[T], num: Numeric[T]): Cluster[T] = {  
		require( xt.nonEmpty, "Cluster.moveCenter Cannot relocate time series datapoint" )
		
		if( members.size <= 0)
		   throw new IllegalStateException("Cluster.stdDev this cluster has no member")
  	 
			// Compute the sum of the value of each dimension of the time series ... 
			// The matrix observations x features has to be transposed in order to 
			// compute the sum of observations for each feature
		val sums = transpose(members.map( xt(_)).toList).map(_.sum)
							
			// then average it by the number  of data points in the cluster
		Cluster[T](sums.map( _ / members.size))
	}
	
	

   
		/**
		 * Compute the standard deviation of the members of this cluster from its center.
		 * @param xt time series used in the computation of the center
		 * @param distance metric used to measure the distance between the center and any of the 
		 * member of the cluster.
		 * @throws IllegalArgumentException if the time series argument is undefined
		 * @throws IllegalStateException if there are no member in the cluster
		 * @return standard deviation of all the members from the center of the cluster.
		 */
	@throws(classOf[IllegalArgumentException])
	@throws(classOf[IllegalStateException])
	final def stdDev(xt: XVSeries[T], distance: DistanceFunc[T]): Double = {
		require( xt.nonEmpty,
			"Cluster.stdDev Cannot compute the standard deviation wih  undefined times series")
	  
		if( members.size <= 0)
			throw new IllegalStateException("Cluster.stdDev this cluster has no member")
		
			// Extract the vector of the distance between each data
			// point and the current center of the cluster.
		val ts = members.map( xt( _))			     	// convert a vector
									.map( distance(center, _))	// compute the distance between point and center
								  .toVector
									
			// Compute the standard deviation of the distance between the
			// data points and the center of the cluster using the Stats class
		Stats[Double](ts).stdDev
	}
   
		/**
		 * Returns the list of index of the data points (members) that belong to this cluster
		 * @return list of index of the members of this clusters
		 */
	@inline
	final def getMembers: List[Int] = members.toList
	
			/**
			 * Textual representation of a cluster for debugging purpose.
			 * @return String representation of this cluster
			 */
	override def toString: String = {
			// First collect list of observations members to this cluster
		val membersList = members.mkString("\t   ")
			// then collect the values of the centroid (center) vector
		val centerString = center.map(show(_)).mkString(" ")
		s"Cluster definition\nCentroids: $centerString\nMembership: $membersList"
	}
	
	private def show(x: Double): String = format(x, emptyString, SHORT)
}


		/**
		 * Companion object for the Cluster class. The singleton is used
		 * to define the constructor for this cluster.
		 * @author Patrick Nicolas
		 * @since February 22, 2014
		 * @note Scala for Machine Learning Chapter 4 Unsupervised learning / K-means clustering
		 */
object Cluster {
	type DistanceFunc[T] = (DblArray, Array[T]) => Double
  
		/**
		 * Default constructor for a cluster
		 * @param center Initial centroid for this cluster
		 */
	def apply[T <: AnyVal](center: DblArray)(implicit f: T => Double): Cluster[T] = new Cluster[T](center)
  
		/**
		 * Constructor for a Cluster with undefined center.
		 */
	def apply[T <: AnyVal](implicit f: T => Double): Cluster[T] = new Cluster[T](Array.empty[Double])

}

// ----------------------------  EOF -------------------------------------------
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
package org.scalaml.unsupervised.em

import scala.util.{Try, Success, Failure}
import scala.collection.JavaConversions._

import org.apache.commons.math3.distribution.fitting.MultivariateNormalMixtureExpectationMaximization
import org.apache.commons.math3.distribution.{MixtureMultivariateNormalDistribution, MultivariateNormalDistribution}
import org.apache.commons.math3.exception.{DimensionMismatchException, NumberIsTooSmallException, NumberIsTooLargeException, NotStrictlyPositiveException}
import org.apache.log4j.Logger

import org.scalaml.stats.XTSeries
import org.scalaml.unsupervised.Distance._
import org.scalaml.core.ITransform
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.util.LoggingUtils._
import MultivariateEM._, XTSeries._


		/**
		 * Case class that define a cluster or Gaussian mixture for the expectation-maximization
		 * algorithm
		 * @param key Key or identifier for the cluster
		 * @param means Array of mean values for this cluster
		 * @param density Density or standard deviation for the cluster
		 * @author Patrick Nicolas
		 * @since 0.99 June 19, 2015
		 */
case class EMCluster(key: Double, means: DblArray, density: DblArray)



		/**
		 * Class that implements the Multivariate Expectation-Maximization algorithm with 
		 * K number of clusters. The class uses the Multivariate Normal distribution, Mixture
		 * of Gaussian Distribution and the Expectation Maximization algorithm from the Apache
		 * Commons Math library.
		 * @tparam T type of features of observation
		 * @constructor Instantiate a Multivariate Expectation Maximization for time series of data 
		 * point of type Array{T]. 
		 * @throws IllegalArgumentException if K is out of range
		 * @param K Number of clusters used in the Expectation-Maximization algorithm.
		 * @param xt time series to train the expectation-maximization (Gaussian mixture)
		 * @author Patrick Nicolas
		 * @since 0.98 February 25, 2014
		 * @version 0.99.1
		 * @see Scala for Machine Learning Chapter 4 "Unsupervised learning" Expectation-Maximization
		 * @see Apache Commons Match org.apache.commons.math3.distribution._
		 * @see http://commons.apache.org/proper/commons-math/
		 */
final class MultivariateEM[T <: AnyVal](K: Int, xt: XVSeries[T])(implicit f: T => Double) 
		extends ITransform[Array[T]](xt) with Monitor[Double] { 
	require( K > 0 && K < MAX_K, s"MultivariateEM: Found K $K required [1, $MAX_K]")
	
	protected val logger = Logger.getLogger("MultivariateEM")
	

	type V = EMCluster
	private[this] val model: Option[EMModel] = train
	
	@inline
	final def isModel: Boolean = model.isDefined
	
		/**
		 * Implement the Expectation-Maximization algorithm as a data transformation.
		 * @throws MatchError if the input time series is undefined or have no elements
		 * @return PartialFunction of time series of elements of type T as input to 
		 * Expectation-Maximization algorithm and tuple of type List[EMEntry] as output.
		 */
	override def |> : PartialFunction[Array[T], Try[V]] = {
		case x: Array[T] if x.length == dimension(xt) && isModel =>
			Try ( model.map(_.minBy(c => euclidean(c.means, x))).get)
	}
	
	
	private def train: Option[EMModel] = Try {
				// Convert the time series of observations to a matrix (2)
		val data: DblMatrix = xt  // force a type conversion
		
				// Instantiate the algorithm then compute the estimate step
		val multivariateEM = new EM(data)
		val est = estimate(data, K)
				
				// Extract the mixture of Gaussian distribution from the dataset (3)
		multivariateEM.fit(est)
			
				// Start the M-step by invoking Commons Math getFittedModel (4)
		val newMixture = multivariateEM.getFittedModel
				
				// Convert the Java type of Commons Math lib to Scala types
				// then extracts the key, mean and standard deviation
		newMixture.getComponents.toList
		.map(p => EMCluster(p.getKey, p.getValue.getMeans, p.getValue.getStandardDeviations))	
	}._toOption("MultivariateEM.train failed", logger)

	
	
	override def toString: String = {
		model.map( _.map(c => 
		  s"Mean: ${c.means.mkString(",")}\n density: ${c.density.mkString("," )}").mkString("\n"))
		.getOrElse("No model")
	}
}


		/**
		 * Companion object for the Multivariate Expectation-Maximization algorithm that defines
		 * internal types EM and EMOutput, the constructor apply and the computation of estimate.
		 * @author Patrick Nicolas
		 * @since February 24, 2014
		 */
object MultivariateEM { 
	private val MAX_K = 500
	
	type EMModel = List[EMCluster]
	
			/**
		 * Shorten description of the type for the Guassian
		 * mixture of expectation maximization defined in Apache Commons Math.
		 */
	type EM = MultivariateNormalMixtureExpectationMaximization
	type EMMixture = MixtureMultivariateNormalDistribution
	
		/**
		 * Default constructor for the Expectation Maximization
		 * @param numComponents Number of clusters or components used in the Expectation-Maximization algorithm
		 * @param xt input time series
		 */
	def apply[T <: AnyVal](numComponents: Int, xt: XVSeries[T])(implicit f: T => Double) = 
			new MultivariateEM[T](numComponents, xt)
	
		/**
		 * Help method to retrieve the estimate for the Multivariate Gaussian mixture model. The
		 * method has protected access so it cannot be used outside the EM package and therefore
		 * does not have to catch Apache Commons Math exception. Those exceptions are caught by the
		 * client code MultivariateEM.|>.
		 * @param data input matrix for the estimate
		 * @param K number of clusters
		 * @throws IllegalArgumenException if the input is not defined or K is out of range.
		 */
	protected def estimate(data: DblMatrix, K: Int): EMMixture = {
		require( !data.isEmpty, 
				"MultivariateEM.estimate Cannot estimate the Gaussian mixture for undefined input")
		require( K > 0 && K < MAX_K, 
				s"MultivariateEM.estimate Number K of clusters for EM $K is out of range")
			
		MultivariateNormalMixtureExpectationMaximization.estimate(data, K)
	}
}


// ------------------------------------------------  EOF ----------------------------------------------------------
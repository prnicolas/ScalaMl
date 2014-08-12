/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.unsupervised.em


import org.scalaml.core.{XTSeries, Types}
import org.scalaml.workflow.PipeOperator
import org.apache.commons.math3.distribution.fitting.MultivariateNormalMixtureExpectationMaximization
import org.apache.commons.math3.distribution.{MixtureMultivariateNormalDistribution, MultivariateNormalDistribution}
import scala.collection.JavaConversions._
import org.apache.commons.math3.exception.{DimensionMismatchException, NumberIsTooSmallException, NumberIsTooLargeException, NotStrictlyPositiveException}


		/**
		 * <p>Class that implements the Multivariate Expectation-Maximization algorithm with 
		 * K number of clusters. The class uses the Multivariate Normal distribution, Mixture
		 * of Gaussian Distribution and the Expectation Maximization algorithm from the Apache
		 * Commons Math library.</p>
		 * @param K number of Clusters used in the EM algorithm
		 * @throws IllegalArgumentException if K is out of range
		 * @author Patrick Nicolas
		 * @since February 25, 2014
		 * @note Scala for Machine Learning
		 */
import Types.ScalaMl._
import MultivariateEM._
import XTSeries._
final class MultivariateEM[T <% Double](val K: Int) extends PipeOperator[XTSeries[Array[T]], EMOutput] { 
	require( K > 0 && K < MAX_K, "Number K of clusters for EM " + K + " is out of range")
	
	type EM = MultivariateNormalMixtureExpectationMaximization
	
		/**
		 * <p>Implement the Expectation-Maximization algorithm as a data transformation.</p>
		 * @param xt time series of vectors (array) of parameterized type
		 * @throws IllegalArgumentException if the input time series is undefined or has zero dimension
		 * @return a list of tuple (key, vector of means, vector of standard deviation for each cluster/components 
		 * if no error is detected, None if any of the following Apache Commons math exceptions, DimensionMismatchException,
		 * NumberIsTooSmallException, NumberIsTooLargeException or NotStrictlyPositiveException is caught
		 */
	override def |> (xt: XTSeries[Array[T]]): Option[EMOutput] = {
	  require(xt != null && xt.toArray.size > 0, "Cannot apply EM on undefined time series")
	  require( dimension(xt) > 0, "Cannot apply EM on time series with zero dimension")
	  
	  val data: DblMatrix = xt  // force a conversion
		
	  try {
		val multivariateEM = new EM(data)
		multivariateEM.fit(estimate(data, K))
			 
		val newMixture = multivariateEM.getFittedModel
	    val components = newMixture.getComponents.toList
 		Some(components.map(p => 
 		   (p.getKey.toDouble, p.getValue.getMeans, p.getValue.getStandardDeviations)
 		))
	  }
	  		// A generic exception has to caught in the case the Apache Commmons Library
	        // is updated and introduces new type of exception
	  catch {
		case e: DimensionMismatchException => Console.println(e.toString); None
		case e: NumberIsTooSmallException => Console.println(e.toString); None
		case e: NumberIsTooLargeException => Console.println(e.toString); None
		case e: NotStrictlyPositiveException => Console.println(e.toString); None
		case e: Exception => Console.println(e.toString); None
	  }
	}
}


	/**
	 * <p>Companion object for the Multivariate Expectation-Maximization algorithm that defines
	 * internal types EM and EMOutput, the constructor apply and the computation of estimate.</p>
	 * @author Patrick Nicolas
	 * @since February 24, 2014
	 */
object MultivariateEM { 
	final val MAX_K = 500
    
    type EMOutput = List[(Double, DblVector, DblVector)]
	def apply[T <% Double](numComponents: Int) = new MultivariateEM[T](numComponents)
	
		/**
		 * <p>Help method to retrieve the estimate for the Multivariate Gaussian mixture model. The
		 * method has protected access so it cannot be used outside the EM package and therefore
		 * does not have to catch Apache Commons Math exception. Those exceptions are caught by the
		 * client code MultivariateEM.|>.</p>
		 * @param data input matrix for the estimate
		 * @param K number of clusters
		 * @throws IllegalArgumenException if the input is not defined or K is out of range.
		 */
	protected def estimate(data: DblMatrix, K: Int): MixtureMultivariateNormalDistribution = {
		require(data != null && data.size > 0, "Cannot eximate the Gaussian mixture distribution for undefined input")
		require( K > 0 && K < MAX_K, "Number K of clusters for EM " + K + " is out of range")
			
		MultivariateNormalMixtureExpectationMaximization.estimate(data, K)
	}
}


// ------------------------------------------------  EOF ----------------------------------------------------------
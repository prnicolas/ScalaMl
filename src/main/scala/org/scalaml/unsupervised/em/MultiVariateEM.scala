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
package org.scalaml.unsupervised.em


import org.scalaml.core.XTSeries
import org.scalaml.core.design.PipeOperator
import org.apache.commons.math3.distribution.fitting.MultivariateNormalMixtureExpectationMaximization
import org.apache.commons.math3.distribution.{MixtureMultivariateNormalDistribution, MultivariateNormalDistribution}
import scala.collection.JavaConversions._
import org.apache.commons.math3.exception.{DimensionMismatchException, NumberIsTooSmallException, NumberIsTooLargeException, NotStrictlyPositiveException}
import scala.util.{Try, Success, Failure}
import org.scalaml.core.types.ScalaMl._
import MultivariateEM._
import XTSeries._
import org.scalaml.util.Display
import org.apache.log4j.Logger

		/**
		 * <p>Class that implements the Multivariate Expectation-Maximization algorithm with 
		 * K number of clusters. The class uses the Multivariate Normal distribution, Mixture
		 * of Gaussian Distribution and the Expectation Maximization algorithm from the Apache
		 * Commons Math library.<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
		 * <b>K</b>   Number of clusters used in the Expectation-Maximization algorithm.</span></pre></p>
		 * @constructor Instantiate a Multivariate Expectation Maximization for time series of data point of type Array{T]. 
		 * @throws IllegalArgumentException if K is out of range
		 * @author Patrick Nicolas
		 * @since February 25, 2014
		 * @note Scala for Machine Learning Chapter 4 Unsupervised learning / Expectation-Maximization
		 */
final class MultivariateEM[T <% Double](K: Int) extends PipeOperator[XTSeries[Array[T]], EMOutput] { 
	require( K > 0 && K < MAX_K, s"MultivariateEM Number K of clusters for EM $K is out of range")
	
	private val logger = Logger.getLogger("MultivariateEM")
	type EM = MultivariateNormalMixtureExpectationMaximization	
	
		/**
		 * <p>Implement the Expectation-Maximization algorithm as a data transformation.</p>
		 * @param xt time series of vectors (array) of parameterized type
		 * @throws MatchError if the input time series is undefined or have no elements
		 * @return PartialFunction of time series of elements of type T as input to Expectation-Maximization algorithm and tuple of type EMOutput as output.
		 */
	override def |> : PartialFunction[XTSeries[Array[T]], EMOutput] = {
		case xt: XTSeries[Array[T]] if(xt != null && xt.size > 0 && dimension(xt) > 0) => {
			val data: DblMatrix = xt  // force a type conversion
		  
			Try {
				val multivariateEM = new EM(data)
				val est = estimate(data, K)
				multivariateEM.fit(est)
				 
				val newMixture = multivariateEM.getFittedModel
				val components = newMixture.getComponents.toList
				components.map(p => 
					(p.getKey.toDouble, p.getValue.getMeans, p.getValue.getStandardDeviations))
			} 
			match {
				case Success(components) => components
				case Failure(e) => Display.error("MultivariateEM.|> ", logger, e); List.empty
			}
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
    
		/**
		 * Type EMOutput as a list of tuple (key, array of means, array of standard deviation.</p>
		 */
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
		require(data != null && data.size > 0, "MultivariateEM.estimate Cannot eximate the Gaussian mixture distribution for undefined input")
		require( K > 0 && K < MAX_K, s"MultivariateEM.estimate Number K of clusters for EM $K is out of range")
			
		MultivariateNormalMixtureExpectationMaximization.estimate(data, K)
	}
}


// ------------------------------------------------  EOF ----------------------------------------------------------
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
 * Version 0.99
 */
package org.scalaml.app.chap4

import scala.util.Try
import org.apache.log4j.Logger

import org.scalaml.stats.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.DataSource
import org.scalaml.unsupervised.clustering.{Cluster, KMeans, KMeansConfig}
import org.scalaml.unsupervised.Distance._
import org.scalaml.plots.Legend
import org.scalaml.util.{DisplayUtils,  LoggingUtils}
import LoggingUtils._, YahooFinancials._,  ScalaMl._, DisplayUtils._, KMeans._
import org.scalaml.app.Eval

		/**
		 * '''Purpose:''' Singleton to evaluate the KMeans algorithm
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see org.scalaml.unsupervised.kmeans
		 * @see Scala for Machine Learning Chapter 4 ''Unsupervised learning'' Clustering / K-means
		 */
object KMeansEval extends UnsupervisedLearningEval {

		/**
		 * Name of the evaluation 
		 */
	val name: String = "KMeansEval"
	
	private val START_INDEX = 80
	private val NUM_SAMPLES = 50
	private val MAX_ITERS = 250
	val NORMALIZE = true

		/**
		 * Execution of the scalatest for '''KMeans''' class
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate
		 * 	
		 * Exceptions thrown during the execution of the tests are caught by the wrapper or handler
		 * test method in Eval trait defined as follows:
		 * {{{
		 *    def test(args: Array[String]) =
		 *      Try(run(args)) match {
		 *        case Success(n) => ...
		 *        case Failure(e) => ...
		 * }}}
		 * The tests can be executed through ''sbt run'' or individually by calling 
		 * ''TestName.test(args)'' (i.e. DKalmanEval.test(Array[String]("IBM") )
		 * @param args array of arguments used in the test
		 */
	@throws(classOf[IllegalArgumentException])
	override protected def run(args: Array[String]): Int = {
		require(args.length > 0, s"KMeansEval Argument for number of cluster undefined")

		import scala.language.postfixOps
		
			// Incorrect format throws an exception that is caught by the eval.test handler
		val KValues: Array[Int] = args.map(_.toInt)

		show(s"$header Evaluation of K-means clustering")
		type XVSeriesSet = Array[XVSeries[Double]]
      
			// Nested function to compute the density of K clusters generated from a 
			// set of observations obs. The condition on the argument are caught 
			// by the K-means constructor.
		def density(K: Int, obs: XVSeries[Double]): DblVector = {
			val kmeans = KMeans[Double](KMeansConfig(K, MAX_ITERS), obs)
			kmeans.density.getOrElse(Vector.empty[Double])
		}
			
			
			// Extract the price of the security using a data source
		def getPrices: Try[XVSeriesSet] = Try {
			symbolFiles.map( DataSource(_, path, NORMALIZE, 1) |> extractor )
					.filter( _.isSuccess ).map( _.get)
		}
	
			// Extract a subset of observed prices
		def getPricesRange(prices: XVSeriesSet) = 
			prices.view.map(_.head.toArray).map( _.drop(START_INDEX).take(NUM_SAMPLES))
												
			// Basic computation pipeline 
		(for {
				// Retrieve the stocks' prices
			prices <- getPrices
			
				// Retrieve the stocks' price variation ranges
			values <- Try { getPricesRange(prices) }
			
				// Compute the density of the clusters
			stdDev <- Try { KValues.map( density(_, values.toVector)) }
			
				// Generates the partial function for this K-means transformation
			pfnKmeans <- Try { KMeans[Double](KMeansConfig(5, MAX_ITERS), values.toVector) |> }
			
				// Generate the clusters
			if( pfnKmeans.isDefinedAt(values.head))
				predict <- pfnKmeans(values.head)
		} yield {		
			
				// Display training profile
			profile(values.toVector)
			val results = s"""Daily price for ${prices.size} stocks
							| Clusters density\n${stdDev.mkString("\n")}""".stripMargin
			show(results)
		})
		.getOrElse(error("failed to train K-means"))
	}
	
	private def profile(obs: XVSeries[Double]): Boolean = {
		val legend = Legend("Density", 
						"K-means clustering convergence", "Recursions", "Number of reasigned data points")
		KMeans[Double](KMeansConfig(8, MAX_ITERS), obs).display("Reasigned", legend)
	}
	

	private def toString(clusters: List[Cluster[Double]]): String = 
		clusters.zipWithIndex.map { case(c, n) => {
			val membership = c.getMembers.map(m => 
			  							symbolFiles(m).substring(0, symbolFiles(m).indexOf(".")-1))
			s"\nCluster#$n => ${membership.mkString(",")}"
		}}.mkString(" ")
}


// -----------------------------------  EOF ---------------------------------------------------
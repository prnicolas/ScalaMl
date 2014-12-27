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
package org.scalaml.scalability.spark


import org.apache.spark.mllib.clustering.{KMeans, KMeansModel}
import org.apache.spark.mllib.linalg.DenseVector
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

import scala.annotation.implicitNotFound

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.XTSeries
import org.scalaml.core.Design.PipeOperator

		/**
		 * <p>Define the configuration of the Spark KMeans wrapper.</p>
		 * @constructor Create a configuration for the Spark K-means algorithm.
		 * @param K Number of clusters used in Spark KMeans
		 * @param numIter  Maximum number of iterations allowed for Spark KMeans
		 * @param numRuns  Number of runs to be executed by Spark KMeans.
		 * @throws IllegalArgumentException if any of the parameters is out of range
		 * @author Patrick Nicolas
		 * @since April, 2, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks/Apache Spark
		 */
final class SparkKMeansConfig(K: Int, maxNumIters: Int, numRuns: Int =1) {
	import SparkKMeansConfig._
  
	check(K, maxNumIters, numRuns)
	
		/**
		 * <p>Reference to MLlib KMeans class that is initialized with the class parameters</p>
		 */
	val kmeans: KMeans = {
		val kmeans = new KMeans
		kmeans.setK(K)
		kmeans.setMaxIterations(maxNumIters)
		kmeans.setRuns(numRuns)
		kmeans
	}
}

		/**
		 * <p>Companion object for the Spark K-means configuration class. The singleton
		 * defines the constructors and validate its parameters.</p>
		 * @author Patrick Nicolas
		 * @since April 2, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks / Apache Spark & MLlib
		 *
		 */
object SparkKMeansConfig {	
	private val MAX_NUM_CLUSTERS = 500
	private val MAX_NUM_ITERS = 250
	private val MAX_NUM_RUNS = 500
	
	private def check(K: Int, maxNumIters: Int, numRuns: Int): Unit = {
		require( K > 0 && K  < MAX_NUM_CLUSTERS, "Number of clusters K $K is out of range")
		require( maxNumIters > 0 && maxNumIters  < MAX_NUM_ITERS, 
				s"Maximum number of iterations $maxNumIters is out of range")
		require( numRuns > 0 && numRuns  < MAX_NUM_RUNS, 
				s"Maximum number of runs for K-means $numRuns is out of range")
	}
}

// --------------------------------------  EOF ---------------------------------------------------
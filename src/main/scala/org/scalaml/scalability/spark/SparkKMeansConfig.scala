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
package org.scalaml.scalability.spark


import org.apache.spark.mllib.clustering.{KMeans, KMeansModel}
import org.apache.spark.mllib.linalg.DenseVector
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

import scala.annotation.implicitNotFound

import org.scalaml.core.types.ScalaMl._
import org.scalaml.core.XTSeries
import org.scalaml.core.design.PipeOperator



		/**
		 * <p>Define the configuration of the Spark KMeans wrapper.<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
		 * <b>K</b>         Number of clusters used in Spark KMeans
		 * <b>numIters</b>  Maximum number of iterations allowed for Spark KMeans
		 * <b>numRuns</b>   Number of runs to be executed by Spark KMeans.
		 * </span></pre></p>
		 * @constructor Create a configuration for the Spark K-means algorithm.
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


object SparkKMeansConfig {	
	private def check(K: Int, maxNumIters: Int, numRuns: Int): Unit = {
		require( K > 0 && K  < 500, "Number of clusters K $K is out of range")
		require( maxNumIters > 0 && maxNumIters  < 500, s"Maximum number of iterations $maxNumIters is out of range")
		require( numRuns > 0 && numRuns  < 500, s"Maximum number of runs for K-means $numRuns is out of range")
	}
}

// --------------------------------------  EOF ---------------------------------------------------
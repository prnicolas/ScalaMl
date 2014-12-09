/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97
 */
package org.scalaml.scalability.spark


import org.apache.spark.mllib.clustering.{KMeans, KMeansModel}
import org.apache.spark.mllib.linalg.DenseVector
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

import scala.annotation.implicitNotFound

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.XTSeries
import org.scalaml.core.design.PipeOperator



		/**
		 * <p>Class wrapper for the Spark KMeans implementation. The model is fully generated through
		 * training during instantiation of objects in order to reduce their life-cycle.<br>
		 * The algorithm implements the default data transformation interface, PipeOperator.</p>
		 * @constructor Create a wrapper for the Spark K-means algorithm. 
		 * @throws IllegalArgumentException if the configuration or the time series is undefined.
		 * @param kMeansConfig Configuration of the Spark KMeans<br>
		 * @param rddConfig Configuration parameters for the Spark RDD<br>
		 * @param xt Time series used for the training of the Spark KMeans<br>
		 * @param sc  implicit spark context.
		 * @author Patrick Nicolas
		 * @since April 2, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks / Apache Spark & MLlib
		 */
@implicitNotFound("Spark context is implicitely undefined")
final class SparkKMeans(kMeansConfig: SparkKMeansConfig, rddConfig: RDDConfig, xt: XTSeries[DblVector])(implicit sc: SparkContext) 
			extends PipeOperator[DblVector, Int] {

	import SparkKMeans._
	check(kMeansConfig, rddConfig, xt)
	
	private val model: KMeansModel = kMeansConfig.kmeans.run(RDDSource.convert(xt, rddConfig))
  
		/**
		 * <p>Method that classify a new data point in any of the cluster.</p>
		 * @param values data point to be classify
		 * @throws IllegalArgumentException if the data point is not defined
		 * @return the id of the cluster if succeeds, None otherwise.
		 */
	override def |> : PartialFunction[DblVector, Int] = {
		case x: DblVector if(x != null && x.size > 0 && model != null) =>
			model.predict(new DenseVector(x))
	}
	
		
	override def toString: String = {
		val buf = new StringBuilder
		buf.append(s"K-Means cluster centers from training\nIndex\t\tCentroids\n")
		model.clusterCenters.zipWithIndex.foreach(ctr =>buf.append(s"#${ctr._2}: ${ScalaMl.toString(ctr._1.toArray)}\n"))
		buf.toString
	}
}


		/**
		 * <p>Companion object for the Spark K-means class. The singleton
		 * defines the constructors and validate its parameters.</p>
		 * @author Patrick Nicolas
		 * @since April 2, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks / Apache Spark & MLlib
		 */
object SparkKMeans {
		/**
		 * Default constructor for SparkKMeans class
		 * @param kMeansConfig Configuration of the Spark KMeans<br>
		 * @param rddConfig Configuration parameters for the Spark RDD<br>
		 * @param xt Time series used for the training of the Spark KMeans<br>
		 * @param sc  implicit spark context.
		 */
	def apply(config: SparkKMeansConfig, rddConfig: RDDConfig, xt: XTSeries[DblVector])(implicit sc: SparkContext): SparkKMeans = 
		new SparkKMeans(config, rddConfig, xt)
	
	private def check(kMeansConfig: SparkKMeansConfig, rddConfig: RDDConfig, xt: XTSeries[DblVector]): Unit = {
		require(kMeansConfig != null, "Cannot a K-means model without a stateuration")
		require(rddConfig != null, "Cannot execute a K-means on non-stateured RDD")
		require(xt != null && xt.size > 0, "Cannot execute a K-means on undefined input time series")
	}
}

// --------------------------------------  EOF ---------------------------------------------------
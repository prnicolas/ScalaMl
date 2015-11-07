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
package org.scalaml.scalability.spark

import scala.annotation.implicitNotFound
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.stats.XTSeries
import org.scalaml.core.ITransform
import org.apache.spark.mllib.clustering.{KMeans, KMeansModel}
import org.apache.spark.mllib.linalg.DenseVector
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.scalaml.util.{FormatUtils, DisplayUtils, LoggingUtils}
import LoggingUtils._


		/**
		 * Class wrapper for the Spark KMeans implementation. The model is fully generated through
		 * training during instantiation of objects in order to reduce their life-cycle
		 * 
		 * The algorithm implements the default data transformation interface, PipeOperator.
		 * @constructor Create a wrapper for the Spark K-means algorithm. 
		 * @throws IllegalArgumentException if the configuration or the time series is undefined.
		 * @param kMeansConfig Configuration of the Spark KMeans
		 * @param rddConfig Configuration parameters for the Spark RDD
		 * @param xt Time series used for the training of the Spark KMeans
		 * @param sc  implicit spark context.
		 * @author Patrick Nicolas
		 * @since 0.98 April 2, 2014
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 12 ''Scalable frameworks'' / Apache Spark & MLlib
		 */
@implicitNotFound(msg = "SparkKMeans Spark context is not implicitely defined")
final class SparkKMeans(
		kMeansConfig: SparkKMeansConfig, 
		rddConfig: RDDConfig, 
		xt: Vector[DblArray])
		(implicit sc: SparkContext)	extends ITransform[DblArray](xt) {

	import SparkKMeans._
	
	type V = Int
	check(xt)
	
	private val logger = Logger.getLogger("SparkKMeans")
	
	private[this] val model: Option[KMeansModel] = train

		/**
		 * Method that classify a new data point in any of the cluster.
		 * @param values data point to be classify
		 * @throws IllegalArgumentException if the data point is not defined
		 * @return the id of the cluster if succeeds, None otherwise.
		 */
	override def |> : PartialFunction[DblArray, Try[V]] = {
		case x: DblArray if(x.length > 0 && model != None) => 
			Try[V](model.get.predict(new DenseVector(x)))
	}
	
		
	override def toString: String = {
		val header = "K-Means cluster centers from training\nIndex\t\tCentroids\n"
		model.map( _.clusterCenters
								.zipWithIndex
								./:(new StringBuilder(header))((b, ctr) => 
									b.append(s"#${ctr._2}: ${FormatUtils.format(ctr._1.toArray)}\n")).toString)
				 .getOrElse("Model undefined")
	}
	
	private def train: Option[KMeansModel] = 
		Try(kMeansConfig.kmeans.run(RDDSource.convert(xt, rddConfig))).toOption
}


		/**
		 * Companion object for the Spark K-means class. The singleton
		 * defines the constructors and validate its parameters.
		 * @author Patrick Nicolas
		 * @since April 2, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks / Apache Spark & MLlib
		 */
object SparkKMeans {
		/**
		 * Default constructor for SparkKMeans class
		 * @param kMeansConfig Configuration of the Spark KMeans
		 * @param rddConfig Configuration parameters for the Spark RDD
		 * @param xt Time series used for the training of the Spark KMeans
		 * @param sc  implicit spark context.
		 */
	def apply(
			config: SparkKMeansConfig, 
			rddConfig: RDDConfig, 
			xt: Vector[DblArray])
			(implicit sc: SparkContext): SparkKMeans = 
				new SparkKMeans(config, rddConfig, xt)
	
	
			/**
		 * Default constructor for SparkKMeans class
		 * @param kMeansConfig Configuration of the Spark KMeans
		 * @param rddConfig Configuration parameters for the Spark RDD
		 * @param xt Time series used for the training of the Spark KMeans
		 * @param sc  implicit spark context.
		 */
	def apply(
			config: SparkKMeansConfig, 
			rddConfig: RDDConfig, 
			xt: DblMatrix)
			(implicit sc: SparkContext): SparkKMeans = 
				new SparkKMeans(config, rddConfig, xt.toVector)
	
	private def check(xt: Vector[DblArray]): Unit = {
		require( !xt.isEmpty, "SparkKMeans.check input time series for Spark K-means is undefined")
	}
}

// --------------------------------------  EOF ---------------------------------------------------
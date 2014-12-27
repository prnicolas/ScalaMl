/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.scalability.spark


import scala.annotation.implicitNotFound
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.XTSeries
import org.scalaml.core.Design.PipeOperator
import org.apache.spark.mllib.clustering.{KMeans, KMeansModel}
import org.apache.spark.mllib.linalg.DenseVector
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.scalaml.util.{FormatUtils, DisplayUtils}




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
@implicitNotFound("SparkKMeans Spark context is not implicitely defined")
final class SparkKMeans(
		kMeansConfig: SparkKMeansConfig, 
		rddConfig: RDDConfig, 
		xt: XTSeries[DblVector])
		(implicit sc: SparkContext)	extends PipeOperator[DblVector, Int] {

	import SparkKMeans._
	check(xt)
	
	private val logger = Logger.getLogger("SparkKMeans")
	
	private[this] val model: Option[KMeansModel] = train match {
		case Success(kmeansModel) => Some(kmeansModel)
		case Failure(e) => DisplayUtils.none("SparkKMeans.model cannot be initiated", logger, e)
	}

  
		/**
		 * <p>Method that classify a new data point in any of the cluster.</p>
		 * @param values data point to be classify
		 * @throws IllegalArgumentException if the data point is not defined
		 * @return the id of the cluster if succeeds, None otherwise.
		 */
	override def |> : PartialFunction[DblVector, Int] = {
		case x: DblVector if(!x.isEmpty && model != None) => model.get.predict(new DenseVector(x))
	}
	
		
	override def toString: String = {
		val header = "K-Means cluster centers from training\nIndex\t\tCentroids\n"
		model.map( _.clusterCenters
								.zipWithIndex
								.foldLeft(new StringBuilder(header))((b, ctr) => 
									b.append(s"#${ctr._2}: ${FormatUtils.format(ctr._1.toArray)}\n")).toString)
				 .getOrElse("Model undefined")
	}
	
	private def train: Try[KMeansModel] = 
		Try(kMeansConfig.kmeans.run(RDDSource.convert(xt, rddConfig)))

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
	def apply(
			config: SparkKMeansConfig, 
			rddConfig: RDDConfig, 
			xt: XTSeries[DblVector])
			(implicit sc: SparkContext): SparkKMeans = 
				new SparkKMeans(config, rddConfig, xt)
	
	private def check(xt: XTSeries[DblVector]): Unit = {
		require( !xt.isEmpty, "SparkKMeans.check input time series for Spark K-means is undefined")
	}
}

// --------------------------------------  EOF ---------------------------------------------------
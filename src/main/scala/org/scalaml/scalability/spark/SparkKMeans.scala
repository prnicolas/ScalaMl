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
			 * <p>Class wrapper for the Spark KMeans implementation. The model is fully generated through
			 * training during instantiation of objects in order to reduce their life-cycle.<br>
			 * The algorithm implements the default data transformation interface, PipeOperator.<br>
			 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
			 * <b>kMeansConfig</b>  Configuration of the Spark KMeans
			 * <b>rddConfig</b>     Configuration parameters for the Spark RDD
			 * <b>xt</b>            Time series used for the training of the Spark KMeans
			 * <b>sc</b>            implicit spark context.
			 * </span></pre></p>
			 * @constructor Create a wrapper for the Spark K-means algorithm. 
			 * @throws IllegalArgumentException if the configuration or the time series is undefined.
			 * 
			 * @author Patrick Nicolas
			 * @since April 2, 2014
			 * @note Scala for Machine Learning Chapter 12 Scalable frameworks/Apache Spark
			 */
@implicitNotFound("Spark context is implicitely undefined")
final class SparkKMeans(kMeansConfig: SparkKMeansConfig, rddConfig: RDDConfig, xt: XTSeries[DblVector])(implicit sc: SparkContext) 
			extends PipeOperator[DblVector, Int] {

	import SparkKMeans._
	check(kMeansConfig, rddConfig, xt)
	
	private val model = kMeansConfig.kmeans.run(RDDSource.convert(xt, rddConfig))
  
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
}



object SparkKMeans {
	def apply(config: SparkKMeansConfig, rddConfig: RDDConfig, xt: XTSeries[DblVector])(implicit sc: SparkContext): SparkKMeans = 
		new SparkKMeans(config, rddConfig, xt)
	
	private def check(kMeansConfig: SparkKMeansConfig, rddConfig: RDDConfig, xt: XTSeries[DblVector]): Unit = {
		require(kMeansConfig != null, "Cannot a K-means model without a stateuration")
		require(rddConfig != null, "Cannot execute a K-means on non-stateured RDD")
		require(xt != null && xt.size > 0, "Cannot execute a K-means on undefined input time series")
	}
}

// --------------------------------------  EOF ---------------------------------------------------
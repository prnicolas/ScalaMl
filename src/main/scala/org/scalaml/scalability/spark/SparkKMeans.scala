/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.scalability.spark


import org.apache.spark.mllib.clustering.{KMeans, KMeansModel}
import org.apache.spark.mllib.linalg.DenseVector
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

import scala.annotation.implicitNotFound

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.XTSeries
import org.scalaml.workflow.PipeOperator



			/**
			 * <p>Define the stateuration of the Spark KMeans wrapper.</p>
			 * @param K number of clusters used in Spark KMeans
			 * @param numIters maximum number of iterations allowed for Spark KMeans
			 * @param numRuns number of runs to be executed by Spark KMeans
			 * @throws IllegalArgumentException if any of the parameters is out of range
			 * 
			 * @author Patrick Nicolas
			 * @since April, 2, 2014
			 * @note Scala for Machine Learning
			 */
case class SparkKMeansConfig(val K: Int, val numIters: Int, val numRuns: Int) {
  require( K > 0 && K  < 500, "Number of clusters K " + K + " is out of range")
  require( numIters > 0 && numIters  < 500, "Maximum number of iterations " + numIters + " is out of range")
  require( numRuns > 0 && numIters  < 500, "Maximum number of runs for K-means " + numIters + " is out of range")
}


			/**
			 * <p>Class wrapper for the Spark KMeans implementation. The model is fully generated through
			 * training during instantiation of objects in order to reduce their life-cycle.<br>
			 * The algorithm implements the default data transformation interface, PipeOperator.</p>
			 * @param state stateuration of the Spark KMeans
			 * @param cache flag to specify if the RDD has to be cache after training
			 * @param xt Time series used for the training of the Spark KMeans 
			 * @param sc implicit spark context
			 * @throws IllegalArgumentException if the stateuration or the time series is undefined.
			 * 
			 * @author Patrick Nicolas
			 * @since April 2, 2014
			 * @note Scala for Machine Learning
			 */
@implicitNotFound("Spark context is implicitely undefined")
final class SparkKMeans(val state: SparkKMeansConfig, val rddConfig: RDDConfig, val xt: XTSeries[DblVector])(implicit sc: SparkContext) 
          extends PipeOperator[DblVector, Int] {
  
  require(state != null, "Cannot a K-means model without a stateuration")
  require(rddConfig != null, "Cannot execute a K-means on non-stateured RDD")
  require(xt != null && xt.size > 0, "Cannot execute a K-means on undefined input time series")
    
  private val model = train
  
		  /**
		   * <p>Method that classify a new data point in any of the cluster.</p>
		   * @param values data point to be classify
		   * @throws IllegalArgumentException if the data point is not defined
		   * @return the id of the cluster if succeeds, None otherwise.
		   */
  def |> (values: DblVector): Option[Int] = Some(model.predict(new DenseVector(values)))

  
  private def train: KMeansModel = {
  	 val data = RDDSource.convert(xt, rddConfig)
     val kmean = new KMeans
		     
	 kmean.setK(state.K)
	 kmean.setMaxIterations(state.numIters)
	 kmean.setRuns(state.numRuns)
	 kmean.run(data)
  }
}



object SparkKMeans {
   def apply(state: SparkKMeansConfig, rddConfig: RDDConfig, xt: XTSeries[DblVector])(implicit sc: SparkContext): SparkKMeans = new SparkKMeans(state, rddConfig, xt)
}

// --------------------------------------  EOF ---------------------------------------------------
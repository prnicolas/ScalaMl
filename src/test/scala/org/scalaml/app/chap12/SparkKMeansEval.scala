/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.app.chap12

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.storage.StorageLevel

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.scalability.spark.{SparkKMeansConfig, RDDConfig, SparkKMeans}
import org.scalaml.util.DisplayUtils
import org.scalaml.app.Eval


		/**
		 * <p><b>Purpose</b>: Singleton to evaluate the Apache Spark/MLlib
		 * K-means algorithm.</p>
		 * 
		 * @author Patrick Nicolas 
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks / Apache Spark / MLlib
		 */
object SparkKMeansEval extends Eval {
	import scala.util.{Try, Success, Failure}
	import org.apache.log4j.{Logger, Level}
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "SparkKMeansEval"

	private val K = 8
	private val NRUNS = 16
	private val MAXITERS = 200
	private val PATH = "resources/data/chap12/CSCO.csv"
	private val CACHE = true
	
		/**
		 * <p>Execution of the scalatest for SparkKMeans class. This method is invoked by the 
		 * actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header MLLib K-means on Spark framework", logger)
  	
		Try {
			val input = extract
			val volatilityVol = input(0).zip(input(1))
										.map( x => Array[Double](x._1, x._2))
			
				// Disable Info for the Spark logger.
			Logger.getRootLogger.setLevel(Level.ERROR)
			val sparkConf = new SparkConf().setMaster("local[8]")
											.setAppName("SparkKMeans")
											.set("spark.executor.memory", "2048m")
											
			val config = new SparkKMeansConfig(K, MAXITERS, NRUNS)
			implicit val sc = new SparkContext(sparkConf)  // no need to load additional jar file
	
			val rddConfig = RDDConfig(CACHE, StorageLevel.MEMORY_ONLY)
			val sparkKMeans = SparkKMeans(config, rddConfig, XTSeries[DblVector](volatilityVol))
			
			DisplayUtils.show(s"\n${sparkKMeans.toString}\nPrediction:\n", logger)
			val obs = Array[Double](0.23, 0.67)
			val clusterId1 = sparkKMeans |> obs
			DisplayUtils.show(s"(${obs(0)},${obs(1)}) => Cluster #$clusterId1", logger)

			val obs2 = Array[Double](0.56, 0.11)
			val clusterId2 = sparkKMeans |> obs2 
			DisplayUtils.show(s"(${obs2(0)},${obs2(1)}) => Cluster #$clusterId2", logger)
			
			// SparkContext is cleaned up gracefully
			sc.stop
			DisplayUtils.show("Completed", logger)
		}
		match {
			case Success(n) => n
			case Failure(e) => failureHandler(e)
		}
	}
  
  
	private def extract: List[DblVector] = {
		import org.scalaml.trading.YahooFinancials
		import org.scalaml.workflow.data.DataSource

		val extractors = List[Array[String] => Double](
			YahooFinancials.volatility, YahooFinancials.volume 
		)	
		DataSource(PATH, true) |> extractors
	}
}

// ---------------------------------  EOF -------------------------
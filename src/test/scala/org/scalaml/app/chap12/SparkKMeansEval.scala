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
package org.scalaml.app.chap12

import scala.util.{Try, Success, Failure}
import scala.language.postfixOps


import org.apache.log4j.{Logger, Level}
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.storage.StorageLevel

import org.scalaml.stats.XTSeries
import org.scalaml.core.Types.ScalaMl.{DblVector, DblArray, XVSeries}
import org.scalaml.scalability.spark.{SparkKMeansConfig, RDDConfig, SparkKMeans}
import org.scalaml.util.{DisplayUtils,  LoggingUtils}
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.DataSource
import LoggingUtils._, XTSeries._
import org.scalaml.app.Eval


		/**
		 * '''Purpose''': Singleton to evaluate the Apache Spark/MLlib
		 * K-means algorithm.
		 * 
		 * @author Patrick Nicolas 
		 * @see Scala for Machine Learning Chapter 12 "Scalable frameworks" / Apache Spark / MLlib
		 */
object SparkKMeansEval extends Eval {

	
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
		 * Execution of the scalatest for SparkKMeans class. This method is invoked by the 
		 * actor-based test framework function, ScalaMlTest.evaluate
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
	override protected def run(args: Array[String]): Int = {
		show(s"$header MLLib K-means on Spark framework")
  	
				// Disable Info for the Spark logger.
		Logger.getRootLogger.setLevel(Level.ERROR)
		val sparkConf = new SparkConf().setMaster("local[8]")
												.setAppName("SparkKMeans")
												.set("spark.executor.memory", "2048m")
		implicit val sc = new SparkContext(sparkConf)  // no need to load additional jar file

		extract.map( input => {
			val volatilityVol = zipToSeries(input._1, input._2)

			val config = new SparkKMeansConfig(K, MAXITERS, NRUNS)
			
			val rddConfig = RDDConfig(CACHE, StorageLevel.MEMORY_ONLY)
			val sparkKMeans = SparkKMeans(config, rddConfig, volatilityVol)
				
			show(s"\n${sparkKMeans.toString}\nPrediction:\n")
			val obs = Array[Double](0.23, 0.67)
			val clusterId1 = sparkKMeans |> obs
			show(s"(${obs(0)},${obs(1)}) => Cluster #$clusterId1")
	
			val obs2 = Array[Double](0.56, 0.11)
			val clusterId2 = sparkKMeans |> obs2 
			val result = s"(${obs2(0)},${obs2(1)}) => Cluster #$clusterId2"
			show(result)
		})
			
			// SparkContext is cleaned up gracefully
		sc.stop
		1

	}
  
  
	private def extract: Option[(DblVector, DblVector)] = {
		val extractors = List[Array[String] => Double](
			YahooFinancials.volatility, 
			YahooFinancials.volume 
		)	
		val pfnSrc = DataSource(PATH, true) |>
		
		pfnSrc( extractors ) match {
			case Success(res) => Some((res(0).toVector, res(1).toVector))
			case Failure(e) => { error(e.toString); None }
		}
	}
}

// ---------------------------------  EOF -------------------------
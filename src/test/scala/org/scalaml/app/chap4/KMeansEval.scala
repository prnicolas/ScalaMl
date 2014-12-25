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
package org.scalaml.app.chap4

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.{ScalaMl, CommonMath}
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.DataSource
import org.scalaml.unsupervised.clustering.{Cluster, KMeans}
import org.scalaml.unsupervised.Distance.euclidean
import org.scalaml.util.DisplayUtils
import org.scalaml.app.Eval

		/**
		 * <p><b>Purpose:</b> Singleton to evaluate the KMeans algorithm</p>
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning Chapter 4 Unsupervised learning / Clustering / K-means
		 */
object KMeansEval extends UnsupervisedLearningEval {
	import scala.util.{Try, Success, Failure}
	import org.apache.log4j.Logger
	import YahooFinancials._,  ScalaMl._

		/**
		 * Name of the evaluation 
		 */
	val name: String = "KMeansEval"
	
	private val START_INDEX = 70
	private val NUM_SAMPLES = 42

		/**
		 * <p>Execution of the scalatest for <b>KMeans</b> class
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	override def run(args: Array[String]): Int = {
		import CommonMath._
        
		DisplayUtils.show(s"$header Evaluation of K-means clustering", logger)
      
			// nested function to generate K clusters from a set of observations observations
			// obs. The condition on the argument are caught by the K-means constructor.
		def run(K: Int, obs: DblMatrix): Unit = {
			require( !obs.isEmpty, s"$name.run Observations are undefined")
			
			val kmeans = KMeans[Double](K, 150)
			val clusters = kmeans |> new XTSeries[DblVector]("x", obs)
			
			val clustersDesc = KMeansEval.toString(clusters)
			DisplayUtils.show(s"$name with $K clusters\nMembership\n${clustersDesc}", logger)		    	                
			clusters.foreach(c => DisplayUtils.show(s"\n${c.toString}", logger))

			DisplayUtils.show(s"\n$name Cluster density:", logger)
			clusters.map( _.stdDev(XTSeries[DblVector](obs), euclidean))
							.foreach( DisplayUtils.show( _ , logger) )
		}

		val normalize = true
		Try {
			require(symbolFiles.size > 0, s"$name.run The input symbol files are undefined")

			val prices: Array[List[DblVector]] = symbolFiles.map(s => 
					DataSource(s, path, normalize) |> extractor
			)

			prices.find ( _.isEmpty ).map(_ => { 
				DisplayUtils.error(s"$name Could not load data", logger)
			}).getOrElse({ 
			  	val values: DblMatrix = prices.map(x => x(0))
												.map( _.drop(START_INDEX)
												.take(NUM_SAMPLES))
					args.map(_.toInt).foreach(run(_, values))
					DisplayUtils.show(s"$name run completed ", logger)
			})
		} 
		match {
			case Success(n) => n
			case Failure(e) => failureHandler(e)
		}
	}
	
	private def toString(clusters: List[Cluster[Double]]): String = {
		var count = 1
		
		clusters.foldLeft(new StringBuilder)((b, c) => {
			b.append(s"Cluster$count:  ")
			count += 1
			b.append(c.getMembers.foldLeft(new StringBuilder)((b2, mbr) => {
					val symbol = symbolFiles(mbr).substring(0, symbolFiles(mbr).indexOf(".")-1)
							b2.append(s"$symbol\t")
			}).toString).append("\n")
		}).toString
	}
}

// -----------------------------------  EOF ---------------------------------------------------
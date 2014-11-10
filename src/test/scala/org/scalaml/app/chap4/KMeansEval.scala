/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95c
 */
package org.scalaml.app.chap4

import org.scalaml.core.{types, XTSeries}
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.{DataSource, DataSink}
import YahooFinancials._
import org.scalaml.util.Display
import org.apache.log4j.Logger
import scala.util.{Try, Success, Failure}


		/**
		 * <p>Singleton to evaluate the KMeans algorithm</p>
		 * @author Patrick Nicolas
		 * @since February 3, 2014
		 * @note Scala for Machine Learning
		 */
object KMeansEval extends UnsupervisedLearningEval {
   import org.scalaml.unsupervised.clustering.KMeans
   import org.scalaml.unsupervised.Distance.euclidean
   import types.ScalaMl._
   
   final val START_INDEX = 70
   final val NUM_SAMPLES = 42
   
   private val logger = Logger.getLogger("KMeansEval")
  
   override def run(args: Array[String]): Int = {
      import types.CommonMath._
        
      Display.show("Evaluation of K-means clustering", logger)
      
      		// nested function to generate K clusters from a set of observations observations
            // obs. The condition on the argument are caught by the K-means constructor.
      def run(K: Int, obs: DblMatrix): Unit = {
         require(obs != null, "KMeansEval.run observations are undefined")
		 val kmeans = KMeans[Double](K, 150)
		 
		 val clusters = kmeans |> new XTSeries[DblVector]("x", obs)
  		 val descriptor = clusters.foldLeft(new StringBuilder)((b, c) => 
		    b.append(c.getMembers.foldLeft(new StringBuilder)((b2, mbr) => b2.append(s"${symbolFiles(mbr)}, ")).toString).append("\n")
		 )
		 Display.show(s"${descriptor.toString}\nmeans:\n", logger)		    	                
		 clusters.foreach(c => Display.show(c.toString, logger))

		 Display.show("\nCluster standard deviation:\n", logger)
		 clusters.map( _.stdDev(XTSeries[DblVector](obs), euclidean))
		         .foreach( Display.show( _ , logger) )
      }

      val normalize = true
      Try {
         require(symbolFiles.size > 0, "KMeansEval.run The input symbol files are undefined")
         
         val prices: Array[List[DblVector]] = symbolFiles.map(s => DataSource(s, path, normalize) |> extractor)
         prices.find ( _ == List.empty) match {
      	    case Some(nullObsList) => Display.error("Could not load data", logger)
      	    case None => {
      	       val values: DblMatrix = prices.map(x => x(0)).map( _.drop(START_INDEX).take(NUM_SAMPLES))
      		   args.map(_.toInt).foreach(run(_, values))
      		   Display.show("KMeansEval.run ", logger)
      	     }
         }
      } match {
       	 case Success(n) => n
       	 case Failure(e) => Display.error("KMeansEval.run ", logger, e)
      }
   }
}


object KMeansApp extends App {
   KMeansEval.run(Array[String]("2", "3", "4", "7", "9", "10", "13", "15"))
}


// -----------------------------------  EOF ---------------------------------------------------
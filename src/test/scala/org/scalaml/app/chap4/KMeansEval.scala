/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.g.
 */
package org.scalaml.app.chap4

import org.scalaml.core.{Types, XTSeries}
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.{DataSource, DataSink}
import YahooFinancials._
import org.scalaml.util.Display
import org.apache.log4j.Logger



		/**
		 * <p>Singleton to evaluate the KMeans algorithm</p>
		 * @author Patrick Nicolas
		 * @since February 3, 2014
		 * @note Scala for Machine Learning
		 */
object KMeansEval extends UnsupervisedLearningEval {
   import org.scalaml.unsupervised.clustering.KMeans
   import org.scalaml.unsupervised.Distance.euclidean
   
   import Types.ScalaMl._
   
   final val START_INDEX = 80
   final val NUM_SAMPLES = 50
   
   private val logger = Logger.getLogger("KMeansEval")
  
   override def run(args: Array[String]): Unit = {
      import Types.CommonMath._
        
      Display.show("Evaluation of K-means clustering", logger)
      
      		// nested function to generate K clusters from a set of observations observations
            // obs. The condition on the argument are caught by the K-means constructor.
      def run(K: Int, obs: DblMatrix): Unit = {
		val kmeans = KMeans[Double](K, 150)
		
		kmeans |> XTSeries[DblVector](obs) match {
		  case Some(clusters) => {
  		     val descriptor = clusters.foldLeft(new StringBuilder)((b, c) => 
		        b.append(c.getMembers.foldLeft(new StringBuilder)((b2, mbr) => b2.append(symbols(mbr)).append(", ")).toString).append("\n")
		     )
		     Display.show(descriptor.toString + "\nmeans:\n", logger)		    	                
		     clusters.foreach( _.center.foreach( Display.show( _ , logger)))

		     Display.show("\nCluster standard deviation:\n")
		     clusters.map( _.stdDev(XTSeries[DblVector](obs), euclidean)).foreach( Display.show( _ , logger) )
		  } 
		  case None => Display.error("error for K-means run", logger)
        }
      }

      val normalize = true
      val obsList = symbols.map( s => DataSource(s, path, normalize) |> extractor)
      obsList.find ( _ == None) match {
      	 case Some(nullObsList) => Display.error("Could not load data", logger)
      	 case None => {
      		 val values = obsList.head.get.head.drop(START_INDEX).take(NUM_SAMPLES)
      		 args.map(_.toInt) foreach(run(_, values))
      	 }
      }
   }
}


// -----------------------------------  EOF ---------------------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap4

import org.scalaml.core.{Types, XTSeries}
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.{DataSource, DataSink}
import YahooFinancials._



		/**
		 * <p>Singleton to evaluate the KMeans++ algorithm</p>
		 * @author Patrick Nicolas
		 */
object KMeansEval extends UnsupervisedLearningEval {
   import org.scalaml.unsupervised.clustering.KMeans
   import org.scalaml.unsupervised.Distance.euclidean
   
   import Types.ScalaMl._
   
   final val START_INDEX = 80
   final val NUM_SAMPLES = 50
  
   override def run(args: Array[String]): Unit = {
      import Types.CommonMath._
      require(args != null && args.length > 0, "Cannot evaluate K-Means with undefined arguments")
      
      println("Evaluation of K-means clustering")
      
      		// nested function to generate K clusters from a set of observations observations
            // obs. The condition on the argument are caught by the K-means constructor.
      def run(K: Int, obs: DblMatrix): Unit = {
		val kmeans = KMeans[Double](K, 150)
		
		kmeans |> XTSeries[DblVector](obs) match {
		  case Some(clusters) => {
  		     val descriptor = clusters.foldLeft(new StringBuilder)((b, c) => 
		        b.append(c.getMembers.foldLeft(new StringBuilder)((b2, mbr) => b2.append(symbols(mbr)).append(", ")).toString).append("\n")
		     )
		     Console.println(descriptor.toString + "\nmeans:\n")		    	                
		     clusters.foreach( _.center.foreach( println( _ )))

		     Console.println("\nCluster standard deviation:\n")
		     clusters.map( _.stdDev(XTSeries[DblVector](obs), euclidean)).foreach( println )
		  } 
		  case None => Console.println("error")
        }
      }

      val normalize = true
      val obsList = symbols.map( s => DataSource(s, path, normalize) |> extractor)
      obsList.find ( _ == None) match {
      	 case Some(nullObsList) => Console.println("Could not load data")
      	 case None => {
      		 val values = obsList.head.get.head.drop(START_INDEX).take(NUM_SAMPLES)
      		 args.map(_.toInt) foreach(run(_, values))
      	 }
      }
   }
}


// -----------------------------------  EOF ---------------------------------------------------
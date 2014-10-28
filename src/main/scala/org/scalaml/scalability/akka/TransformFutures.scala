/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.94
 */
package org.scalaml.scalability.akka


import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ArrayBuffer
import org.scalaml.core.types.ScalaMl._
import akka.actor._
import akka.util.Timeout
import org.scalaml.core.XTSeries
import org.scalaml.core.design.PipeOperator
import XTSeries._
import org.scalaml.util.Display
import org.apache.log4j.Logger

		/**
		 * <p>Version of the future to compute the variance of data assigned to each group for which the
		 *  the execution blocks until all the future complete the computation of 
		 *  the variance for each assigned fold..</p>
		 *  @param data time series {x, y} used in the cross validation and to be broken down into normalized groups
		 *  @param numFutures number of futures used in the parallelization of the computation
		 *  @throws IllegalArgumentException if the class parameters are either undefined or out of range.
		 *  
		 *  @author Patrick Nicolas
		 *  @since March 30, 2014
		 *  @note Scala for Machine Learning
		 */			
final class TransformFutures(xt: DblSeries, fct: PipeOperator[DblSeries, DblSeries], partitioner: Partitioner) extends Actor {
  implicit val timeout = Timeout(2000)	
  private val logger = Logger.getLogger("TransformFutures")
  
  def receive = {
	  case Start => aggregate(transform)
      case _ => Display.show("Message not recognized", logger)
  }
  
  def transform: Array[Future[DblSeries]] = {   
  	 val partIdx = partitioner.split(xt)
     val partitions = partIdx.map(n => XTSeries[Double](xt.slice(n - partIdx(0), n)))

	 val futures = new Array[Future[DblSeries]](partIdx.size)
	      
	 partitions.zipWithIndex.foreach(pi => {
		futures(pi._2) = Future[DblSeries] { 
		   fct |> pi._1
		 }
	   }) 
	   futures
	}
	
		/**
		 * <p>Delegates the computation of the variance of each group
		 * to their respective future. The execution blocks using Await.</p>
		 * @param futures set of futures used to compute the variance of each grou[
		 * @throws IllegalArgumentException if futures are undefined
		 */
   def aggregate(futures: Array[Future[DblSeries]]): Unit = {
  	  require(futures != null && futures.size > 0, "Cannot delegate computation to undefined futures")
  	  
      val frequenciesList = futures.map(
                 Await.result(_, timeout.duration).toArray  ).toList
      val listFrequencies = frequenciesList.transpose
      Display.show(listFrequencies.map( _.sum), logger)
   }
}



// ------------------------  EOF -----------------------------------------------------------------------------
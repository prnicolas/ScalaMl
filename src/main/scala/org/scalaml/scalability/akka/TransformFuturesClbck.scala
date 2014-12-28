/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.scalability.akka

	// Scala std.lib
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
	// 3rd Party lib
import org.apache.log4j.Logger
	// ScalaMl classes
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.XTSeries
import org.scalaml.core.Design.PipeOperator
import org.scalaml.scalability.akka.message._
import org.scalaml.util.DisplayUtils
import XTSeries._



		/**
		 * <p>Generic distributed transformation of time series using futures and callback methods.</p>
		 *  @constructor Create a distributed transformation for time series. 
		 *  @throws IllegalArgumentException if the class parameters are either undefined or out of range.
		 *  @param xt Time series to be processed
		 *  @param fct Data transformation of type PipeOperator
		 *  @param partitioner Methodology to partition a time series in segments or partitions to 
		 *  be processed by workers.
		 *  
		 *  @author Patrick Nicolas
		 *  @since March 30, 2014
		 *  @note Scala for Machine Learning Chapter 12 Scalable Frameworks/Akka/Futures
		 */			
abstract class TransformFuturesClbck(
		xt: DblSeries, 
		fct: PipeOperator[DblSeries, DblSeries], 
		partitioner: Partitioner) extends Controller(xt, fct, partitioner) {
  
	private val logger = Logger.getLogger("TransformFuturesClbck")
	
		/**
		 * <p>Message handling for the future-based controller for the transformation of 
		 * time series.</p>
		 * <b>Start</b> to initiate the future computation of transformation of time series.<.p>
		 */
	override def receive = {
		case Start => compute(transform)
		case _ => DisplayUtils.error("TransformFuturesClbck.recieve Message not recognized", logger)
	}
  
		/*
		 * Create a future transform by creating an array of 
		 * futures to execute the data transform (or pipe operator)
		 * for each partition.
		 */
	private def transform: Array[Future[DblSeries]] = {   
		val partIdx = partitioner.split(xt)
		val partitions = partIdx.map(n => XTSeries[Double](xt.slice(n - partIdx(0), n).toArray))
			
			// Once the futures are created, they are assigned to
			// execute the data transform fct to their related partition.
		val futures = new Array[Future[DblSeries]](partIdx.size)
		partitions.zipWithIndex.foreach(pi => {
			futures(pi._2) = Future[DblSeries] { fct |> pi._1 }
		}) 
		futures
	}
	
		/**
		 * <p>Executes the aggregation of the results for all the future execution of 
		 * the transformation of time series, using the transform fct.</p>
		 * @param futures Set of future transformation of time series using the transform fct.
		 * @throws IllegalArgumentException if futures are undefined
		 */
	private def compute(futures: Array[Future[DblSeries]]): Seq[Double] = {
		require( !futures.isEmpty, 
				"TransformFuturesClbck.compute Cannot delegate computation to undefined futures")
  	  
		val aggregation = new ArrayBuffer[DblSeries]
			// Listen for notification from any of the futures
		
		futures.foreach(f => {
				// If the notification is a success, then aggregate the results
			f onSuccess {  
				case data: DblSeries => aggregation.append(data)
			}
				// If the notification is a failure, process the exception
				// and append an empty time series to the existing results.
			f onFailure {	
				case e: Exception => {
					DisplayUtils.error("TransformFuturesClbck.aggregate failed", logger, e)
					aggregation.append(XTSeries.empty[Double])
				}
			}
		}) 
			// Returns the aggregated results only if all the partition were
			// successfully processed.
		aggregation.find( _.isEmpty)
							.map( _ => aggregate(aggregation.toArray))
							.getOrElse(Seq.empty)
	}
	
		/**
		 * <p>Executes the aggregation of the results for all the future execution of 
		 * the transformation of time series, using the transform fct.</p>
		 * @param results of the distributed processing of the time series by futures
		 * @throws IllegalArgumentException if the results are undefined
		 */
	protected def aggregate(results: Array[DblSeries]): Seq[Double] 
}

// ------------------------  EOF -----------------------------------------------------------------------------
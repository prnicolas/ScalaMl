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
 * Version 0.99.1
 */
package org.scalaml.scalability.akka

	// Scala std.lib
import scala.util.Try
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

	// 3rd Party lib
import org.apache.log4j.Logger

	// ScalaMl classes
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.ETransform
import org.scalaml.scalability.akka.message._
import org.scalaml.util.LoggingUtils._
import Controller._



		/**
		 * Generic distributed transformation of time series using futures and callback methods.
		 *  @constructor Create a distributed transformation for time series. 
		 *  @throws IllegalArgumentException if the class parameters are either undefined or out of 
		 *  range.
		 *  @param xt Time series to be processed
		 *  @param fct Data transformation of type PipeOperator
		 *  @param nPartitions Number of segments or partitions to
		 *  be processed by workers.
		 *  
		 *  @author Patrick Nicolas
		 *  @since 0.98 March 30, 2014
		 *  @see Scala for Machine Learning Chapter 12 Scalable Frameworks/Akka/Futures
		 */			
abstract class TransformFuturesClbck[T] (
		xt: DblVector,
		fct: PfnTransform, 
		nPartitions: Int) extends Controller(xt, fct, nPartitions) with Monitor[Double] {
  import TransformTypes._
  
	protected val logger = Logger.getLogger("TransformFuturesClbck")

		/**
		 * Message handling for the future-based controller for the transformation of 
		 * time series.
		 * '''Start''' to initiate the future computation of transformation of time series.<.p>
		 */
	override def receive = {  case Start => compute(transform) }
  
		/*
		 * Create a future transform by creating an array of 
		 * futures to execute the data transform (or pipe operator)
		 * for each partition.
		 */
	private def transform: Array[Future[DblVector]] = {   
		val partitions = partition
			
			// Once the futures are created, they are assigned to
			// execute the data transform fct to their related partition.
		val futures = new Array[Future[DblVector]](nPartitions)
		partition.zipWithIndex.foreach{ case( x, n) =>
			futures(n) = Future[DblVector] { fct(x).get }
		}
		futures
	}
	
		/**
		 * Executes the aggregation of the results for all the future execution of 
		 * the transformation of time series, using the transform fct.
		 * @param futures Set of future transformation of time series using the transform fct.
		 * @throws IllegalArgumentException if futures are undefined
		 */
	private def compute(futures: Array[Future[DblVector]]): Seq[Double] = {
		require( futures.length > 0, 
				"TransformFuturesClbck.compute Cannot delegate computation to undefined futures")
  	  
		val buffer = new ArrayBuffer[DblVector]
			// Listen for notification from any of the futures
		
		futures.foreach(f => {
				// If the notification is a success, then aggregate the results
			f onSuccess {  
				case data: DblVector => buffer.append(data)
			}
				// If the notification is a failure, process the exception
				// and append an empty time series to the existing results.
			f onFailure {	
				case e: Exception =>
					error("TransformFuturesClbck.aggregate failed")
					buffer.append(Vector.empty[Double])
			}
		}) 
			// Returns the aggregated results only if all the partition were
			// successfully processed.
		buffer.find( _.isEmpty)
							.map( _ => reduce(buffer))
							.getOrElse(Seq.empty)
	}
	
		/**
		 * Executes the aggregation of the results for all the future execution of 
		 * the transformation of time series, using the transform fct.
		 * @param results of the distributed processing of the time series by futures
		 * @throws IllegalArgumentException if the results are undefined
		 */
	protected def reduce(results: Iterable[DblVector]): Seq[Double] 
}

// ------------------------  EOF -----------------------------------------------------------------------------
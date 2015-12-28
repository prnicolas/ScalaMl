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
package org.scalaml.scalability.akka

	// Scala standard library
import scala.util.Try
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration

	// 3rd party libraries
import akka.actor._
import akka.util.Timeout
import org.apache.log4j.Logger

	// ScalaMl classes
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.scalability.akka.message._
import org.scalaml.core.ETransform
import org.scalaml.util.LoggingUtils._
import Controller._


		/**
		 * Generic implementation of the distributed transformation of time series using 
		 * blocking futures.
		 * @constructor Create a distributed transformation for time series. 
		 * @throws IllegalArgumentException if the class parameters are either undefined or out of range.
		 * @param xt Time series to be processed
		 * @param fct Data transformation of type PipeOperator
		 * @param partitioner Methodology to partition a time series in segments or partitions to be 
		 * processed by workers.
		 * @see org.scalaml.scalability.akka.Controller
		 * 
		 * @author Patrick Nicolas
		 * @since March 30, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable Frameworks / Akka / Futures
		 */			
abstract class TransformFutures(
		xt: DblVector, 
		fct: PfnTransform, 
		nPartitions: Int)
		(implicit timeout: Timeout) extends Controller(xt, fct, nPartitions) with Monitor[Double] {
		
  import TransformTypes._
	protected val logger = Logger.getLogger("TransformFutures")
		/**
		 * Message handling for the future-based controller for the transformation of time series.
		 * '''Start''' to initiate the future computation of transformation of time series.<.p>
		 */
	override def receive = {
		case s: Start => compute(transform)
		case _ => show("TransformFutures.receive Message not recognized")
	}
  
		/*
		 * Create a future transform by creating an array of 
		 * futures to execute the data transform (or pipe operator)
		 * for each partition.
		 */
	private def transform: Array[Future[DblVector]] = {
			// Retrieve the relative index for each partition
	//	val partIdx = partitioner.split(xt)
		
			// Create the partitions
	//	val partitions: Iterable[DblVector] = partIdx.map(n => (xt.slice(n - partIdx(0), n).toVector))


			// Create an array of futures to apply the data
			// transform 'fct' to each partition pi._1
		val futures = new Array[Future[DblVector]](nPartitions)
		partition.zipWithIndex.foreach { case (x, n) => {
			futures(n) = Future[DblVector] { fct(x).get }
		}}
		futures
	}
	
		/**
		 * Executes the aggregation of the results for all the future execution of 
		 * the transformation of time series, using the transform fct.
		 * @param futures Set of future transformation of time series using the transform fct.
		 * @throws IllegalArgumentException if futures are undefined
		 */
	private def compute(futures: Array[Future[DblVector]]): Seq[Double] = {
		require(!futures.isEmpty, "TransformFutures.compute Undefined futures")
 
			// Block until either all the future complete or the execution time-out
		reduce( futures.map(Await.result(_, timeout.duration)) )
	}
	
	
		/**
		 * Executes the aggregation of the results for all the future execution of 
		 * the transformation of time series, using the transform fct.
		 * @param results of the distributed processing of the time series by futures
		 * @throws IllegalArgumentException if the results are undefined
		 */
	protected def reduce(results: Array[DblVector]): Seq[Double] 
}


// ------------------------  EOF -----------------------------------------------------------------------------
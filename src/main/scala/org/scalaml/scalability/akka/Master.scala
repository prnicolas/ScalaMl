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

	// Scala standard library
import scala.util.{Try, Random}
import scala.collection._

	// 3rd party libraries
import org.apache.log4j.Logger
import akka.actor.{Props, PoisonPill, Terminated}

	// ScalaMl classes
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.stats.Stats
import org.scalaml.core.ETransform
import org.scalaml.util.{LoggingUtils, FormatUtils}
import org.scalaml.scalability.akka.message._
import Controller._, LoggingUtils._


		/**
		 * Generic implementation of the distributed transformation of time series using a 
		 * master-worker (or master-slave) design.
		 *  @constructor Create a distributed transformation for time series.
		 *  @throws IllegalArgumentException if the class parameters are either undefined or 
		 *  out-of-range.
		 *  @param xt Time series to be processed
		 *  @param fct Data transformation of type PipeOperator
		 *  @param nPartitions Number of segments or partitions to be processed by workers.
		 *  @see org.scalaml.scalability.akka.Controller
		 *  
		 *  @author Patrick Nicolas
		 *  @since March 30, 2014
		 *  @see Scala for Machine Learning Chapter 12 Scalable Frameworks/Master-workers
		 */		
abstract class Master(
		xt: DblVector, 
		fct: PfnTransform, 
		nPartitions: Int) 
			extends Controller(xt, fct, nPartitions) with Monitor[Double] {

	protected val logger = Logger.getLogger("Master")
	
	
			// Aggregation for results from each worker actors	
	protected[this] val aggregator = new Aggregator(nPartitions)
	
			// The master is responsible for creating the array of worker actors
			// It uses the Akka actor system.
	private[this] val workers = List.tabulate(nPartitions)(n => 
			context.actorOf(Props(new Worker(n, fct)), name = s"worker_$n"))
		
		// This master watches the termination of its worker..
	workers.foreach( context.watch ( _ ) )
 		
	override def preStart(): Unit = show("Master.preStart")
	override def postStop(): Unit = show("Master postStop")
   
		/**		 
		 * Message processing handler of the master actor for a distributed transformation of 
		 * time series.
		 * 
		 * - '''Start''' to partition the original time series and launch data transformation on 
		 * worker actors
		 * 
		 * - '''Completed''' aggregates the results from all the worker actors.
		 */
	override def receive = {
			// Partition the original time series
		case s: Start => start()
		
			// Process completed message from any of the worker
		case msg: Completed =>
			
			// If all workers have returned their results....
			// aggregate the results using a user defined function 
			// and finally stop the worker actors before the master stop itself
			if( aggregator +=  msg.xt) 
				workers.foreach( context.stop(_) )

			// Get notification from worker that they were terminated.
		case Terminated(sender) =>
				// If all the workers have been stopped, then the
				// master stop itself and finally shutdown the system.
			if( aggregator.completed ) {
				show("Master stops and shutdown system")
				context.stop(self)
				context.system.shutdown()
		  }
		case _ => error("Master: Message not supported")
	}

		/*
		 * Split the original time series using the partitioner helper class
		 * then send each time series partition (or segment) to each worker
		 * using the Activate message.
		 */
	private def start(): Unit = {
		show("Master.receive => Start")
				
			// Broadcast the Activate message
		workers.zip(partition.toVector).foreach { case (w, s) => w ! Activate(0, s) }
	}
}

// ---------------------------------  EOF -------------------------
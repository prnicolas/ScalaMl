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

import scala.util.{Random, Properties, Try}
import scala.collection.mutable.ListBuffer

import org.apache.log4j.Logger
import akka.actor.{ActorRef, Props, Actor, actorRef2Scala, PoisonPill, Terminated}
import akka.util.Timeout
import akka.routing._

import org.scalaml.stats.XTSeries
import org.scalaml.core.ETransform
import org.scalaml.core.Types.ScalaMl.{DblVector, DblArray}
import org.scalaml.scalability.akka.message.{Start, Completed, Activate, Terminate}
import org.scalaml.filtering.dft.DFT
import org.scalaml.util.{FormatUtils, LoggingUtils}
import Controller._, LoggingUtils._



		/**
		 * Generic implementation of the distributed transformation of time series using a 
		 * master-worker and router.

		 *  The routing actor is defined by the class '''akka.routing.RoundRobinRouter''' for Akka 
		 *  2.2.4 and earlier version. This class is deprecated in Akka 2.3.4 and later and 
		 *  should be replaced by the class'''akka.routing.RoundRobinPool.'''
		 *  @constructor Create a distributed transformation for time series using actors and a 
		 *  routing actor.
		 *  @throws IllegalArgumentException if the class parameters are either undefined or 
		 *  out of range.
		 *  @param xt Time series to be processed
		 *  @param fct Data transformation of type PipeOperator
		 *  @param partitioner Methodology to partition a time series in segments or partitions to be 
		 *  processed by workers.
		 * 	@param aggr User defined function for aggregating results from a group of worker actors
		 *  @see org.scalaml.scalability.akka.Controller
		 *  
		 *  @author Patrick Nicolas
		 *  @since 0.98.1 March 30, 2014
		 *  @version 0.99
		 *  @see Scala for Machine Learning Chapter 12 "Scalable Frameworks" Master-workers
		 */		
abstract class MasterWithRouter(
		xt: DblVector, 
		fct: PfnTransform, 
		nPartitions: Int) extends Controller(xt, fct, nPartitions) with Monitor[Double] {	

	protected val logger = Logger.getLogger("MasterWithRouter")
	
		// Aggregation for results from each worker actors
	protected[this] val aggregator = new Aggregator(nPartitions)
	
			/*
			 * Akka version 2.3.4 and higher
			 * Create a routing/supervising actor for an array of worker actors.
			 * For Akka version 2.3.4 and higher, the RoundRobinPool should be used as follows
			 * private val router = context.actorOf(Props(new Worker(0, DFT[Double]))
			 *     withRouter(RoundRobinPool(partitioner.numPartitions, 
			 *              supervisorStrategy = this.supervisorStrategy))
			 */
	private[this] val router = { 
			// If Akka version 2.2.6 and earlier
		val routerConfig = RoundRobinRouter(nPartitions, supervisorStrategy = this.supervisorStrategy)
				
			// if Akka version is 2.3.4 or higher
		/*
		val routerConfig = RoundRobinPool(partitioner.numPartitions, supervisorStrategy = 
				this.supervisorStrategy)
		*/
		context.actorOf(Props(new Worker(0, fct)).withRouter(routerConfig) )
	}
		// The master is watching the router/supervisor to all workers
		// to be terminated.
	context.watch(router)
		
		
		/**
		 * Message processing handler for the routing master for a distributed transformation 
		 * of time series:
		 * 
		 * - '''Start''' to partition the original time series and launch data transformation on 
		 * worker actors
		 * - '''Completed''' aggregates the results from all the worker actors.
		 */
	override def receive = {
			// Partition the original time series
		case msg: Start => start
		
			// If all workers have returned their results....
			// aggregate the results using a user defined function 
			// and finally stop the router supervisor before the master stops itself..
		case msg: Completed => {
		  if( aggregator += msg.xt)
		  	context.stop(router)  // Alternative: router ! Terminate
		}
		
					// Get notification from worker that they were terminated.
		case Terminated(sender) => {
				// If all the workers have been stopped, then the
				// master stop itself and finally shutdown the system.
		  
			if( aggregator.completed ) {
				show("Master stops and shutdown system")
				context.stop(self)
				context.system.shutdown
		  }
		}
		
		case _ => error("MasterWithRouter.receive Message not recognized")
	} 

		/*
		 * Split the original time series using the partitioner helper class
		 * then send each time series partition (or segment) to each worker
		 * using the Activate message.
		 */
	private def start: Unit = {
		show("MasterWithRouter.receive => Start")
		partition.toVector.foreach { router ! Activate(0, _) }
	}
	
	
	final private def isNewVersion: Boolean = akka.actor.ActorSystem.Version(2).toInt > 2
}

// --------------------------------  EOF -------------------------------------
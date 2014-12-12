/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.2
 */
package org.scalaml.scalability.akka

import scala.util.{Random, Properties}
import scala.collection.mutable.ListBuffer

import org.apache.log4j.Logger
import akka.actor.{ActorRef, Props, Actor, actorRef2Scala, PoisonPill}
import akka.util.Timeout
import akka.routing._

import org.scalaml.core.XTSeries
import org.scalaml.core.design.PipeOperator
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.scalability.akka.message.{Start, Completed, Activate, Terminate}
import org.scalaml.filtering.DFT
import org.scalaml.util.{FormatUtils, DisplayUtils}
import XTSeries.DblSeries



		/**
		 * <p>Generic implementation of the distributed transformation of time series using a 
		 * master-worker and router.
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;
		 * ,Arial,Helvetica,sans-serif;">
		 *  The routing actor is defined by the class <b>akka.routing.RoundRobinRouter</b> for Akka 2.2.4 and earlier version.<br>
		 *  This class is deprecated in Akka 2.3.4 and later and should be replaced by the class <b>akka.routing.RoundRobinPool.</b></span></pre></p> 
		 *  @constructor Create a distributed transformation for time series using actors and a routing actor.
		 *  @throws IllegalArgumentException if the class parameters are either undefined or out of range.
		 *  @param xt Time series to be processed
		 *  @param fct Data transformation of type PipeOperator
		 *  @param partitioner Methodology to partition a time series in segments or partitions to be 
		 *  processed by workers.
		 * 	@param aggr User defined function for aggregating results from a group of worker actors
		 *  @see org.scalaml.scalability.akka.Controller
		 *  
		 *  @author Patrick Nicolas
		 *  @since March 30, 2014
		 *  @note Scala for Machine Learning Chapter 12 Scalable Frameworks/Master-workers
		 */		
abstract class MasterWithRouter(
		xt: DblSeries, 
		fct: PipeOperator[DblSeries, DblSeries], 
		partitioner: Partitioner, 
		aggr: (List[DblVector]) => Seq[Double]) extends Controller(xt, fct, partitioner) {	

	private val logger = Logger.getLogger("MasterWithRouter")
	private val SLEEP = 1500
	protected val MAX_NUM_DATAPOINTS = 128
		
	protected val aggregator = new ListBuffer[DblVector]
	
			// Akka version 2.3.4 and higher 
			//private val router = context.actorOf(Props(new Worker(0, DFT[Double]))
			//		.withRouter(RoundRobinPool(partitioner.numPartitions, supervisorStrategy = this.supervisorStrategy)))  	
	println(s"Scala version: ${Properties.versionNumberString}, ${Properties.versionMsg}" )
	
	private val router = context.actorOf(Props(new Worker(0, fct))
		.withRouter(RoundRobinRouter(partitioner.numPartitions, 
				supervisorStrategy = this.supervisorStrategy)))
		
		
		/**
		 * <p>Message processing handler for the routing master for a distributed transformation 
		 * of time series.<br>
		 * <b>Start</b> to partition the original time series and launch data transformation on 
		 * worker actors.<br> 
		 * <b>Completed</b> aggregates the results from all the worker actors.</p>
		 */
	override def receive = {
			// Partition the original time series
		case msg: Start => split
		
			// If all workers have returned their results....
			// aggregate the results using a user defined function 
			// and finally stop the router supervisor before the master stops itself..
		case msg: Completed => {
			if(aggregator.size >= partitioner.numPartitions-1) {
				val aggr = aggregate.take(MAX_NUM_DATAPOINTS).toArray
				DisplayUtils.show(s"Aggregated\n${FormatUtils.format(aggr)}", logger)
				
			//	router ! Terminate
				context.stop(self)
			}
			aggregator.append(msg.xt.toArray)
		}
		case _ => DisplayUtils.error("MasterWithRouter.receive Message not recognized", logger)
	} 
	
	
	protected def aggregate: Seq[Double] = aggr(aggregator.toList)

	private def split: Unit = {
		DisplayUtils.show("MasterWithRouter.receive => Start", logger)
		val indices = partitioner.split(xt)
		indices.foreach(n => router ! Activate(0, xt.slice(n - indices(0), n)) )
	}
}

// --------------------------------  EOF -------------------------------------
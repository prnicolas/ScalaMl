/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.2
 */
package org.scalaml.scalability.akka

import scala.util.Random
import scala.collection._

import org.apache.log4j.Logger
import akka.actor.{Props, PoisonPill}

import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.core.XTSeries
import org.scalaml.core.design.PipeOperator
import org.scalaml.stats.Stats
import org.scalaml.util.{DisplayUtils, FormatUtils}
import org.scalaml.scalability.akka.message._
import XTSeries._



		/**
		 * <p>Generic implementation of the distributed transformation of time series using a 
		 * master-worker (or master-slave) design.</p>
		 *  @constructor Create a distributed transformation for time series.
		 *  @throws IllegalArgumentException if the class parameters are either undefined or out of range.
		 *  @param xt Time series to be processed
		 *  @param fct Data transformation of type PipeOperator
		 *  @param partitioner Methodology to partition a time series in segments or partitions to be 
		 *  processed by workers.
		 *  @param aggr User defined function for aggregating results from a group of worker actors
		 *  @see org.scalaml.scalability.akka.Controller
		 *  
		 *  @author Patrick Nicolas
		 *  @since March 30, 2014
		 *  @note Scala for Machine Learning Chapter 12 Scalable Frameworks/Master-workers
		 */		
abstract class Master(
		xt: DblSeries, 
		fct: PipeOperator[DblSeries, DblSeries], 
		partitioner: Partitioner, 
		aggr: (List[DblVector]) => immutable.Seq[Double]) extends Controller(xt, fct, partitioner) {

	private val logger = Logger.getLogger("Master")
	protected val MAX_NUM_DATAPOINTS = 128
	protected val aggregator = new mutable.ListBuffer[DblVector]
	
	private val workers = List.tabulate(partitioner.numPartitions)(n => 
		context.actorOf(Props(new Worker(n, fct)), name = "worker_" + String.valueOf(n)))
   
	override def preStart: Unit = DisplayUtils.show("Master.preStart", logger)
	override def postStop: Unit = DisplayUtils.show("Master postStop", logger)
   
		/**		 
		 * <p>Message processing handler for the rmaster for a distributed transformation of 
		 * time series.<br>
		 * <b>Start</b> to partition the original time series and launch data transformation on 
		 * worker actors.<br> 
		 * <b>Completed</b> aggregates the results from all the worker actors.</p>
		 */
	override def receive = {
			// Partition the original time series
		case s: Start => split
		
			// Process completed message from any of the worker
		case msg: Completed => {
			
			// If all workers have returned their results....
			// aggregate the results using a user defined function 
			// and finally stop the worker actors before the master stop itself
			if(aggregator.size >= partitioner.numPartitions-1) {
				val aggr = aggregate.take(MAX_NUM_DATAPOINTS).toArray
				DisplayUtils.show(s"Aggregated\n${FormatUtils.format(aggr)}", logger)
				
				// Terminate itself after stopping the workers.
				context.stop(self)
			}
			aggregator.append(msg.xt.toArray)
		}
		case _ => DisplayUtils.error("Message not recognized and ignored", logger)
	}
	
	protected def aggregate: immutable.Seq[Double] = aggr(aggregator.toList)

	private def split: Unit = {
		DisplayUtils.show("Master.receive => Start", logger)
		val indices = partitioner.split(xt)
		workers.zip(indices).foreach(w => 
			w._1 ! Activate(0, xt.slice(w._2 - indices(0), w._2)) )  
	}
}

// ---------------------------------  EOF -------------------------
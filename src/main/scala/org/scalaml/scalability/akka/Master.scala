/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96c
 */
package org.scalaml.scalability.akka

import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.Types.ScalaMl._
import java.io.{IOException, PrintWriter}
import akka.actor._
import org.scalaml.stats.Stats
import org.scalaml.util.Display
import org.scalaml.filtering.DFT
import org.scalaml.core.XTSeries
import org.scalaml.core.design.PipeOperator
import XTSeries._
import scala.util.Random
import scala.collection.mutable.ListBuffer
import org.apache.log4j.Logger
import org.scalaml.scalability.akka.message._



		/**
		 * <p>Generic implementation of the distributed transformation of time series using a master-worker (or master-slave) design.</p>
		 *  @constructor Create a distributed transformation for time series.
		 *  @throws IllegalArgumentException if the class parameters are either undefined or out of range.
		 *  @param xt Time series to be processed
		 *  @param fct Data transformation of type PipeOperator
		 *  @param partitioner Methodology to partition a time series in segments or partitions to be processed by workers.
		 * 
		 *  @author Patrick Nicolas
		 *  @since March 30, 2014
		 *  @note Scala for Machine Learning Chapter 12 Scalable Frameworks/Master-workers
		 */		
abstract class Master(xt: DblSeries, fct: PipeOperator[DblSeries, DblSeries], partitioner: Partitioner) 
								extends Controller(xt, fct, partitioner) {

	private val logger = Logger.getLogger("Master")
	val MAX_NUM_DATAPOINTS = 128
    
	private val workers = List.tabulate(partitioner.numPartitions)(n => 
		context.actorOf(Props(new Worker(n, fct)), name = "worker_" + String.valueOf(n)))

	protected val aggregator = new ListBuffer[DblVector]
   
	override def preStart: Unit = Display.show("Master.preStart", logger)
	override def postStop: Unit = Display.show("Master postStop", logger)
   
		/**		 
		 * <p>Message processing handler for the rmaster for a distributed transformation of time series.<br>
		 * <b>Start</b> to partition the original time series and launch data transformation on worker actors.<br> 
		 * <b>Completed</b> aggregates the results from all the worker actors.</p>
		 */
	override def receive = {
		case s: Start => split
		case msg: Completed => {
			if(aggregator.size >= partitioner.numPartitions-1) {
				val aggr = aggregate.take(MAX_NUM_DATAPOINTS).toArray
				Display.show(s"Aggregated\n${ScalaMl.toString(aggr, "", true)}", logger)
				workers.foreach( _ ! PoisonPill)
				
				Thread.sleep(2000)
				context.stop(self)
			}
			aggregator.append(msg.xt.toArray)
		}
		case _ => Display.error("Message not recognized and ignored", logger)
	}
	
	protected def aggregate: Seq[Double] 

	private def split: Unit = {
		Display.show("Master.receive => Start", logger)
		val partIdx = partitioner.split(xt)
		workers.zip(partIdx).foreach(w => w._1 ! Activate(0, xt.slice(w._2-partIdx(0), w._2), self) )  
	}
}

// ---------------------------------  EOF -------------------------
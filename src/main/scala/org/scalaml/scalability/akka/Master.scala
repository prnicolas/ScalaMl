/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.scalability.akka


import org.scalaml.core.types.ScalaMl._
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
		 * <p>Generic implementation of the distributed transformation of time series using a master-worker (or master-slave) design..<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;"> 
		 * <b>xt</b>           Time series to be processed.
		 * <b>fct</b>          Data transformation
		 * <b>partitioner</b>  Method to partition time series for concurrent processing.
		 * </span></pre></p>
		 *  @constructor Create a distributed transformation for time series.
		 *  @throws IllegalArgumentException if the class parameters are either undefined or out of range.
		 *  
		 *  @author Patrick Nicolas
		 *  @since March 30, 2014
		 *  @note Scala for Machine Learning Chapter 12 Scalable Frameworks/Master-workers
		 */		
abstract class Master(xt: DblSeries, fct: PipeOperator[DblSeries, DblSeries], partitioner: Partitioner) 
								extends Controller(xt, fct, partitioner) {

	private val logger = Logger.getLogger("Master")
   
	private val workers = List.tabulate(partitioner.numPartitions)(n => 
		context.actorOf(Props(new Worker(n, fct)), name = "worker_" + String.valueOf(n)))

	protected val aggregator = new ListBuffer[DblVector]
   
	override def preStart: Unit = Display.show("Master.preStart", logger)
	override def postStop: Unit = Display.show("Master stop", logger)
   
		/**		 
		 * <p>Message processing handler for the rmaster for a distributed transformation of time series.<br>
		 * <b>Start</b> to partition the original time series and launch data transformation on worker actors.<br> 
		 * <b>Completed</b> aggregates the results from all the worker actors.</p>
		 */
	override def receive = {
		case Start => split
		
		case msg: Completed => {
			Display.show(s"Master.receive.Completed from worker ${msg.id}", logger)
			
			if(aggregator.size >= partitioner.numPartitions-1) {
				Display.show(aggregate.take(10), logger)
				workers.foreach( _ ! PoisonPill)
				Thread.sleep(1000)
				context.stop(self)
			}
			aggregator.append(msg.xt.toArray)
		}
		case _ => Display.error("Message not recognized and ignored", logger)
	}
	
	protected def aggregate: Seq[Double] 

	private def split: Unit = {
		Display.show("Master.recieve.Start", logger)
		val partIdx = partitioner.split(xt)
		workers.zip(partIdx).foreach(w => w._1 ! Activate(0, xt.slice(w._2-partIdx(0), w._2), self) )  
	}
}

// ---------------------------------  EOF -------------------------
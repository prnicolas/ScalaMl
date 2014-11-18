/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 */
package org.scalaml.scalability.akka


import org.scalaml.core.types.ScalaMl._
import org.scalaml.scalability.akka.message._
import akka.routing.RoundRobinRouter
import akka.actor.{ActorRef, Props, Actor, actorRef2Scala}
import akka.util.Timeout
import scala.util.Random
import org.scalaml.filtering.DFT
import org.scalaml.core.XTSeries
import org.scalaml.core.design.PipeOperator
import XTSeries._
import org.apache.log4j.Logger
import org.scalaml.util.Display
import akka.routing.RoundRobinPool
import scala.collection.mutable.ListBuffer


		/**
		 * <p>Generic implementation of the distributed transformation of time series using a master-worker and router.<br><br>
		 *  <b>xt</b> Time series to be processed.<br> 
		 *  <b>fct</b> Data transformation<br>
		 *  <b>partitioner</b> Method to partition time series for concurrent processing.</p>
		 *  @constructor Create a distributed transformation for time series. 
		 *  @throws IllegalArgumentException if the class parameters are either undefined or out of range.
		 *  
		 *  @author Patrick Nicolas
		 *  @since March 30, 2014
		 *  @note Scala for Machine Learning Chapter 12 Scalable Frameworks/Master-workers
		 */		
abstract class MasterWithRouter(xt: DblSeries, fct: PipeOperator[DblSeries, DblSeries], partitioner: Partitioner) 
							extends Controller(xt, fct, partitioner) {	

	private val logger = Logger.getLogger("MasterWithRouter")
	
	private val aggregator = new ListBuffer[DblVector]
	private val router = context.actorOf(Props(new Worker(0, DFT[Double]))
		.withRouter(RoundRobinPool(partitioner.numPartitions, supervisorStrategy = this.supervisorStrategy)))  	

		/**
		 * <p>Message processing handler for the routing master for a distributed transformation of time series.<br>
		 * <b>Start</b> to partition the original time series and launch data transformation on worker actors.<br> 
		 * <b>Completed</b> aggregates the results from all the worker actors.</p>
		 */
	override def receive = {
		case msg: Start => split

		case msg: Completed => {
			aggregate
			router ! Terminate
			Thread.sleep(1500)
			context.stop(self)
		}
		case _ => Display.error("MasterWithRouter.recieve Message not recognized", logger)
	} 
	
	
	protected def aggregate: Seq[Double]

	private def split: Unit = {
		val partIdx = partitioner.split(xt)
		partIdx.foreach(n => router ! Activate(0, xt.slice(n, n - partIdx(0)), self) )
	}
}

// --------------------------------  EOF -------------------------------------
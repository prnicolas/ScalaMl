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

import scala.util.Random
import scala.collection.mutable.ListBuffer

import org.apache.log4j.Logger
import akka.actor._

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.XTSeries
import org.scalaml.core.Design.PipeOperator
import org.scalaml.scalability.akka.message._
import org.scalaml.stats.Stats
import org.scalaml.util.{FormatUtils, DisplayUtils}
import org.scalaml.filtering.DFT

import XTSeries._


		/**
		 * <p>Worker actor responsible for transforming a time series using the 
		 * PipeOperator |>. The computation is initiated by the Master that acts 
		 * as the workflow controller.</p>
		 * @constructor Create a worker actor. 
		 * @param  id Identifier or counter for the worker actors.
		 * @param fct Data transformation function to be applied to a time series.
		 * 
		 * @author Patrick Nicolas
		 * @since March 24, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable Framework/Akka/Master-workers
		 */
final class Worker(id: Int, fct: PipeOperator[DblSeries, DblSeries]) extends Actor {
	import Worker._
	check(id)
	
	private val logger = Logger.getLogger("WorkerActor")
	override def postStop: Unit = DisplayUtils.show(s"WorkerActor${id}.postStop", logger)
 
		/**
		 * <p>Event loop of the work actor that process two messages:
		 * <ul>
		 *  <li>Activate: to start processing this assigned partition</li>
		 *  <li>Terminate: To stop this worker actor
		 *  </ul>
		 */
	override def receive = {
		case msg: Activate => {
				// Increment the messages id
			val msgId = msg.id+id
			DisplayUtils.show(s"Worker_${id}.receive => Activate message ${msgId}", logger)
			
				// Execute the data transformation
			val output: DblSeries = fct |> msg.xt
			DisplayUtils.show(results(output.take(NUM_DATAPOINTS_DISPLAY)), logger)
			
				// Returns the results for processing this partition
			sender ! Completed(msgId, output)
		}
		case _ => DisplayUtils.error(s"WorkerActor${id}.receive Message not recognized", logger)
	}
	
	private def results(output: XTSeries[Double]): String = {
		val res = output.toArray.foldLeft(new StringBuilder)((b, o) => 
				b.append(s"${FormatUtils.format(o, "", FormatUtils.MediumFormat)} "))
		s"Worker_$id results: ${res.toString}"
	}
}

		/**
		 * Companion object for the worker actor. The singleton is used to validate
		 * the parameters of the Worker class
		 * 
		 * @author Patrick Nicolas
		 * @since March 24, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable Framework/Akka/Master-workers
		 */
object Worker {
	private val NUM_DATAPOINTS_DISPLAY = 12
	
	private def check(id: Int): Unit = require(id >= 0, s"Worker.check Id $id is out of range")
}

// ---------------------------------  EOF -------------------------
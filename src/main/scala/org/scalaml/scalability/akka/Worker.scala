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


import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.Types.ScalaMl
import org.scalaml.scalability.akka.message._
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
	check(id, fct)
	
	private val logger = Logger.getLogger("WorkerActor")
	override def postStop: Unit = Display.show(s"WorkerActor${id}.postStop", logger)
 
		/**
		 * <p>Event loop of the work actor that process two messages<br>
		 * Activate to start processing for the current iteration<br>
		 */
	override def receive = {
		case msg: Activate => {
			val msgId = msg.id+id
			Display.show(s"Worker_${id}.receive => Activate message ${msgId}", logger)
			val output: XTSeries[Double] = transform(msg.xt)
			Display.show(results(output.take(10)), logger)
			msg.sender ! Completed(msgId, output)
		}
		case _ => Display.error(s"WorkerActor${id}.receive Message not recognized", logger)
	}

	private def transform(xt: DblSeries): DblSeries =  fct |> xt
	
	private def results(output: XTSeries[Double]): String = {
		val res = output.foldLeft(new StringBuilder)((b, o) => b.append(s"${ScalaMl.toString(o, "", true)} "))
		s"Worker_$id results: ${res.toString}"
	}
}


object Worker {
	private def check(id: Int, fct: PipeOperator[DblSeries, DblSeries]): Unit = {
		require(id >= 0, s"Worker.check Id $id is out of range")
		require(fct != null, "Worker.check Cannot create a master actor with undefined data transformation function")
	}
}





// ---------------------------------  EOF -------------------------
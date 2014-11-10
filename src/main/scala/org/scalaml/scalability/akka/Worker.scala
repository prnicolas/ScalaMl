/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95c
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



		/**
		 * <p>Worker actor used to implements a generate a value from a two dimension time series.
		 * The computation is initiated by the Master or workflow controller.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since March 24, 2014
		 * @note Scala for Machine Learning
		 */
final class WorkerActor(fct: PipeOperator[DblSeries, DblSeries]) extends Actor {
  private val logger = Logger.getLogger("WorkerActor")
  override def postStop: Unit = Display.show("WorkerActorpostStop", logger)
 
		/**
		 * <p>Event loop of the work actor that process two messages<br>
		 * Activate to start processing for the current iteration<br>
		 * Terminate to shutdown.
		 */
   override def receive = {
      case msg: Activate =>  msg.sender ! Completed(msg.id, transform(msg.xt))
      case _ => Display.show("WorkerActor.receive Message not recognized", logger)
   }
   
   private def transform(xt: DblSeries): DblSeries =  fct |> xt
}


class Partitioner(val numPartitions: Int) {
   def split(xt: DblSeries): Array[Int] = {
  	  val sz = (xt.size.toDouble/numPartitions).floor.toInt
  	  val indices = Array.tabulate(numPartitions)(i=>(i+1)*sz)
  	  indices.update(numPartitions -1, xt.size)
  	  indices
   }
}



// ---------------------------------  EOF -------------------------
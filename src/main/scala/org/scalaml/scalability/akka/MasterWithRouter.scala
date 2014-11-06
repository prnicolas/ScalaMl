/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95
 */
package org.scalaml.scalability.akka


import org.scalaml.core.types.ScalaMl._
import akka.routing.RoundRobinRouter
import akka.actor.{ActorRef, Props, Actor, actorRef2Scala}
import akka.util.Timeout
import scala.util.Random
import org.scalaml.filtering.DFT
import org.scalaml.core.XTSeries
import org.scalaml.core.design.PipeOperator
import XTSeries._


		           

	           
		 /**
		  * <p>Class that control the execution of the gradient for a data set.
		  */
class MasterWithRouter(xt: DblSeries, fct: PipeOperator[DblSeries, DblSeries], partitioner: Partitioner) extends Actor {	
    
   val router = context.actorOf(Props(new WorkerActor(DFT[Double])).withRouter(RoundRobinRouter(nrOfInstances = partitioner.numPartitions)))  	

   override def receive = {
      case msg: Start => split
      
      case msg: Completed => {
         router ! Terminate
	     context.stop(self)
      }
      case _ => println("Message not recognized")
   } 

    private def split: Unit = {
  	   val partIdx = partitioner.split(xt)
  	   partIdx.foreach(n => router ! Activate(xt.slice(n, n - partIdx(0)), self) )
   }
}

// --------------------------------  EOF -------------------------------------
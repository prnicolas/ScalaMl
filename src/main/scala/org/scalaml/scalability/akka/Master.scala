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





class Master(xt: DblSeries, fct: PipeOperator[DblSeries, DblSeries], partitioner: Partitioner) extends Actor {
   require(xt != null && xt.size > 0, "Cannot create a master actor to process undefined data")	
   private val logger = Logger.getLogger("Master")
   
   val workers = List.tabulate(partitioner.numPartitions)(n => 
		    	    context.actorOf(Props(new WorkerActor(fct)), name = "worker_" + String.valueOf(n)))

   val aggregator = new ListBuffer[DblVector]
   
   override def preStart: Unit = Display.show("Master.preStart", logger)
   override def postStop: Unit = Display.show("Master stop", logger)
        /**
   		 * <p>Event loop of the master actor that processes two messages<br>
   		 * Start to initialize the worker actor and launch the normalization of cross validation groups<br>
   		 * Completed to process the results of the current iteration in the balancing procedure.</p>
   		 */
   override def receive = {
      case Start => split
      case msg: Completed => {
      	if(aggregator.size >= partitioner.numPartitions-1) {
		   workers.foreach( _ ! PoisonPill)
           Display.show(aggregator.transpose.map( _.sum), logger)
		   context.stop(self)
         }
      	 aggregator.append(msg.xt.toArray)
	  }
      case _ => Display.error("Message not recognized and ignored", logger)
   }
   
   private def split: Unit = {
  	  val partIdx = partitioner.split(xt)
  	  workers.zip(partIdx).foreach(w => w._1 ! Activate(xt.slice(w._2-partIdx(0), w._2), self) )  
   }
}


object ActorsManagerApp extends App {
   import scala.concurrent.duration.Duration
   import scala.concurrent.{Await, Awaitable}
   import akka.actor.ActorSystem	 
	  
   val NUM_WORKERS = 4
   val NUM_DATA_POINTS = 1000000
   val h = (x:Double) =>2.0*Math.cos(Math.PI*0.005*x) +  // simulated first harmonic
                            Math.cos(Math.PI*0.05*x) +   // simulated second harmonic
                        0.5*Math.cos(Math.PI*0.2*x)  +    // simulated third harmonic 
                        0.2*Random.nextDouble
	  
   val xt = XTSeries[Double](Array.tabulate(NUM_DATA_POINTS)(h(_)))
   val partitioner = new Partitioner(NUM_WORKERS)
   
   implicit val actorSystem = ActorSystem("system") 

   val master = actorSystem.actorOf(Props(new Master(xt, DFT[Double], partitioner)), "Master")
   master ! Start
   Thread.sleep(15000)
   actorSystem.shutdown
}




// ---------------------------------  EOF -------------------------
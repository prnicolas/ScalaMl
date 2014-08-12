/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.scalability.akka


import org.scalaml.core.Types.ScalaMl._
import akka.routing.RoundRobinRouter
import akka.actor.{ActorRef, Props, Actor, actorRef2Scala}
import akka.util.Timeout
import scala.util.Random


		/**
		 * <p>Class that define the message to worker actors to execute 
		 * an operation on a time series.</p>
		 * @param _id  id of the message (incremental counter)
		 * @param data Time series (x,y) of floating point values
		 * @param weights weight factor used in the computation
		 * @param reference to the actor which sent the message
		 * @throws IllegalArgumenException if data, weights or sender are undefined
		 * 
		 * @author Patrick Nicolas
		 * @since March 27, 2014
		 * @note Scala for Machine Learning
		 */
case class Execute(val _id: Int, 
		           val data: XYTSeries, 
		           val weights: XY, 
		           val sender: Actor) {
	
  require( data != null && data.size > 0, "Time series in the execute message is undefined")
  require( weights != null, "Weights used in the computation is undefined in Execute message")
  require( sender != null, "The sender of the Execute message is undefined")
}

		           

	           
		 /**
		  * <p>Class that control the execution of the gradient for a data set.
		  */
class Controller(val numActors: Int,  val data: XYTSeries, val numIters: Int) extends Actor {
   require(numActors > 0 && numActors < 32, "Cannot create a master actor with number of actors " + numActors + " out of range")	
   require(data != null && data.size > 0, "Cannot create a master actor to process undefined data")	
   require(numIters > 0 && numIters < 1000, "Number of iteration for data processing in Master " + numIters + " is out of range")

   val router = context.actorOf(Props(new WorkerActor).withRouter(RoundRobinRouter(nrOfInstances = numActors)))  	
   val normalizer = new GroupsNormalizer(numActors, data)

   override def receive = {
      case msg: Start => execute(0)
      
      case msg: Completed => {
      	 if( normalizer.update(msg.id, msg.variance) ) {
	      	if( msg.id >= numIters) {
	      	   router ! Terminate
	      	   context.stop(self)
	      	}
	      	else 
	      	   execute(msg.id +1)
	     }
      }
      case _ => println("Message not recognized")
   } 
   
  private def execute(id: Int): Unit = 
  	 Range(0, numActors).foreach(n => router ! Activate(id, normalizer.groups(n), self) )
}

// --------------------------------  EOF -------------------------------------
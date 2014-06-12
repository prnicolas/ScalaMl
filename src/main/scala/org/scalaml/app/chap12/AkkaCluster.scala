/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap12


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
		 * @exception IllegalArgumenException if data, weights or sender are undefined
		 * 
		 * @author Patrick Nicolas
		 * @date March 27, 2014
		 * @project Scala for Machine Learning
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
 
	val weights: XY = (Random.nextDouble, Random.nextDouble)
	
    val workers: Array[ActorRef] = {
	   val ar = new Array[ActorRef](numActors)
       Range(0, numActors).foreach( i => {  
      	   val workerActor = new WorkerActor
           val routedActor = context.actorOf(Props(workerActor).withRouter(RoundRobinRouter(nrOfInstances = numActors)))    
           ar.update(i, routedActor)
       })
       ar
    }
    
    override def receive = {
       case Start => {
      	  val segSize = data.size/workers.size
 
      	  workers.zipWithIndex.foreach( w => {
      	  	val lowBound = segSize*w._2
      	    w._1 ! Execute(w._2, data.slice(lowBound, lowBound + segSize), weights, this) })
       }
    }
}

// --------------------------------  EOF -------------------------------------
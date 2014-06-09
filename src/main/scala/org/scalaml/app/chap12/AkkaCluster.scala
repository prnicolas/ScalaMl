/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
/**
 * Classes that illustrates the use of Futures in Scala 2.10.2 and Akka 2.14 framework. The pattern consists of a 
 * main task, 'MainTask' that launches than blocks on a future task 'FutureTask' 
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * 
 * @author Patrick Nicolas
 * @date Feb 3, 2014
 * 
 */
package org.scalaml.app.chap12

import org.scalaml.core.Types

import Types.ScalaMl._


/**
 *  @author Patrick Nicolas
 *  @date Mar 16, 2014
 *  @project Book
 */

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
  require(weights != null, "Weights used in the computation is undefined in Execute message")
  require( sender != null, "The sender of the Execute message is undefined")
}

		           

	           
		           
class Master(val numActors: Int, 
		      val data: XYTSeries, 
		      val gradient: (XY, XY) =>Double,
		      val numIters: Int) extends Actor {
 
	var weights: XY = (Random.nextDouble, Random.nextDouble)
 
    val workers: Array[ActorRef] = {
	   val ar = new Array[ActorRef](numActors)
       Range(0, numActors).foreach( i => {  
      	   val workerActor = new WorkerActor(gradient)
           val routedActor = context.actorOf(Props(workerActor).withRouter(RoundRobinRouter(nrOfInstances = numActors)))    
           ar.update(i, routedActor)
       })
       ar
    }
    
    override def receive = {
       case Start => {
      	  val segSize = data.size/workers.size
          var index: Int  = 0
      	  workers.foreach( x => {
      	    x ! Execute(index, data.slice(index, index + segSize), weights, this)
      	    index += segSize })
       }
    }
}

// --------------------------------  EOF -------------------------------------
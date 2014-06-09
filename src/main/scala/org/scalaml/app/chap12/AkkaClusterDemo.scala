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

/**
 *  @author Patrick Nicolas
 *  @date Mar 16, 2014
 *  @project Book
 */

import akka.routing.RoundRobinRouter
import akka.actor.{ActorRef, Props, Actor, actorRef2Scala}
import akka.util.Timeout
import scala.util.Random

case class Execute(val _id: Int, 
		           val data: Array[(Double, Double)], 
		           val weights: (Double, Double), 
		           val sender: Actor)

class Master(val numActors: Int, 
		      val data: Array[(Double, Double)], 
		      val gradient: ((Double, Double), (Double, Double)) =>Double,
		      val numIters: Int) extends Actor {
 
	var weights: (Double, Double) = (new Random(System.currentTimeMillis).nextDouble, new Random(System.currentTimeMillis).nextDouble)

 
    val workers: Array[ActorRef] = {
       (0 until numActors).foldLeft(new Array[ActorRef](numActors))((ar, i) => {  
           val routedActor = context.actorOf(Props(classOf[WorkerActor], gradient).withRouter(RoundRobinRouter(nrOfInstances = numActors)))    
           ar.update(i, routedActor)
           ar
        })
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
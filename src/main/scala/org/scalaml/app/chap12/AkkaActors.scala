/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap12

import scala.collection.parallel.mutable.ParArray
import scala.collection.mutable.ArraySeq
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import akka.actor.{Actor, ActorRef}
import scala.collection.mutable.ArrayBuffer
import org.scalaml.core.Types

import Types.ScalaMl._


class ExecutionState(val numIters: Int, val numWorkers: Int) {
   require(numIters > 0 && numIters < 1000, "Number of iterations " + numIters + " in execution state in out of range" )
   require(numWorkers > 0 && numWorkers < 32, "Number of worker actors " + numWorkers + " in execution state in out of range" )
      
   var nIters = numIters
   var workersCnt = numWorkers
   
   def decrNumIters: Boolean = { nIters -= 1; nIters == 0 }
   def decrWorkersCnt: Boolean = { workersCnt -= 1; workersCnt == 0 }
   def resetWorkersCnt: Unit = workersCnt = numWorkers
}




		/**
		 * This is a worker actor that execute a process
		 */
final class WorkerActor(val gradient: (XY, XY) =>Double) extends Actor {
   require(gradient != null, "Gradient function in the worker actor is undefined")
	
	
   var data: XYTSeries = null
   override def receive = {
      case msg: Activate => {
         data  = msg.data
         msg.sender ! (Completed(0, data.map( gradient(_, msg.weights)) ))
      }
      case msg: Iterate =>  msg.sender ! Completed(0, data.map( gradient( _, msg.weights)))
      case Terminate => exit
   }
}



class MasterActor(val workers: List[ActorRef], val data: XYTSeries, val numIters: Int) extends Actor {
   require(workers != null && workers.size > 0, "Cannot create a master actor with undefined workers")	
   require(data != null && data.size > 0, "Cannot create a master actor to process undefined data")	
   require(numIters > 0 && numIters < 1000, "Number of iteration for data processing in Master " + numIters + " is out of range")
	   
   import scala.util.Random
      	
   var weights = (Random.nextDouble, Random.nextDouble)
   val execution = new ExecutionState(numIters, workers.size)
   val aggrW = new ArrayBuffer[Double]
   
   override def receive = {
      case msg: Start => initialize    
      case msg: Completed => iterate(msg.w)
   }
      
   private[this] def initialize: Unit = {
      val segSize = data.size/workers.size
      var index: Int  = 0
      workers.foreach( w =>
      	{ w ! Activate(index, data.slice(index, index + segSize), weights, self)
         index += segSize })
   }
   
   private[this] def saveToFile: Unit = {
     import java.io.{PrintWriter, IOException}
      
     var out: PrintWriter = null
     try {
       out = new PrintWriter("output/results")
       out.write(weights.toString)
     }
     catch {
        case ex: IOException => println("error")
     }
     finally {
       if( null != out) {
	       try {
	         out.close
	       }
	       catch {
	         case ex: IOException => println("error")
	       }
       }
       else
         println("output file undefined")
     }
   }
    
   private[this] def iterate(w: Array[Double]): Unit = {
      if( execution.decrNumIters ) {
         workers.foreach( _ ! Terminate )
         saveToFile
         exit
      }
      else {
	      println("Iterate")
	      aggrW ++= w
	      if( execution.decrWorkersCnt ) {
	        val g = aggrW.reduceLeft( _ + _)
	        weights = ( -g*weights._1, -g*weights._2)
	
	        aggrW.clear
	        execution.resetWorkersCnt
	        workers.foreach( _ ! Iterate(execution.numIters, weights, self) )
	      }
      }
   }
}


// ---------------------------------  EOF -------------------------
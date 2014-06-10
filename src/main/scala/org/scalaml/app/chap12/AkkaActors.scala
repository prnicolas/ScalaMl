/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap12


import scala.collection.mutable.ArrayBuffer
import org.scalaml.core.Types.ScalaMl._
import java.io.{IOException, PrintWriter}
import akka.actor._
import scala.util.Random



		/**
		 * <p>Define the state of the execution of a distributed workflow composed
		 * of a Master actor and several worker/slave actors.</p>
		 * @param numIters Number of iterations used in the computation performed by the worker
		 * @param numWorkers Number of workers used in the computation
		 * @exception IllegalArgumenException if the number of iterations or the number of workers is out of range
		 * 
		 * @author Patrick Nicolas
		 * @date March 23, 2014
		 * @project Scala for Machine Learning
		 */
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
		 * <p>Worker actor used to execute a computation 'gradient'. The computation is
		 * initiated by the Master or workflow controller.</p>
		 * @param gradient function to compute the Gradient from a array of (x,y) values
		 * @exception IllegalArgumenException if gradient is undefined
		 * 
		 * @author Patrick Nicolas
		 * @date March 24, 2014
		 * @project Scala for Machine Learning
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
      case Terminate => sys.exit
   }
}



		/**
		 * <p>Master actor that specifies and distributed the computation of gradient
		 * on a data set {x,y}. This purpose of the class is to illustrate the 
		 * Akka actors and message passing mechanism..</p>
		 * @param workers List of reference to the worker or slave actors.
		 * @param data Data set to be processed by the worker actors
		 * @param numIters Maximum number of iterations allowed to the works to 
		 * @exception IllegalArgumenException if gradient is undefined
		 * 
		 * @author Patrick Nicolas
		 * @date March 24, 2014
		 * @project Scala for Machine Learning
		 */
final class MasterActor(val workers: List[ActorRef], val data: XYTSeries, val numIters: Int) extends Actor {
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
         sys.exit
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
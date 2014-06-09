package org.scalaml.app.chap12

import scala.collection.parallel.mutable.ParArray
import scala.collection.mutable.ArraySeq
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import scala.actors.Actor
import scala.collection.mutable.ArrayBuffer
import org.scalaml.core.Types

import Types.ScalaMl._


class ExecutionState(val numIters: Int, val numWorkers: Int) {
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
   
   var data: XYTSeries = null
   def act {
     loop {
        receive {
           case msg: Activate => {
              data  = msg.data
              msg.sender ! Completed(0, data.map(x => gradient(x, msg.weights)) )
           }
           case msg: Iterate =>  msg.sender ! Completed(0, data.map(x => gradient(x, msg.weights)))
           case Terminate => exit
        }
     }
   }
}



class MasterActor(val workers: List[WorkerActor], val data: XYTSeries, val numIters: Int) extends Actor {
   import scala.util.Random
      	
   require(workers != null && workers.length > 0, "Master cannot manage undefined slave actors")
   val rGen = new Random(System.currentTimeMillis)
   var weights = (rGen.nextDouble, rGen.nextDouble)
   val execution = new ExecutionState(numIters, workers.size)
   val aggrW = new ArrayBuffer[Double]
   
   def act {
     loop  {
       receive {
         case msg: Start => initialize    
         case msg: Completed => iterate(msg.w)
       }
     }
   }
      
   private[this] def initialize: Unit = {
      val segSize = data.size/workers.size
      var index: Int  = 0
      workers.foreach( x => { 
         x.start 
         x ! Activate(index, data.slice(index, index + segSize), weights, this); 
         index += segSize
      })
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
	        workers.foreach( _ ! Iterate(execution.numIters, weights, this) )
	      }
      }
   }
}


// ---------------------------------  EOF -------------------------
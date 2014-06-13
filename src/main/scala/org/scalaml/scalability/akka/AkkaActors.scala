/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.scalability.akka


import org.scalaml.core.Types.ScalaMl._
import java.io.{IOException, PrintWriter}
import akka.actor._
import org.scalaml.stats.Stats




		/**
		 * <p>Worker actor used to implements a generate a value from a two dimension time series.
		 * The computation is initiated by the Master or workflow controller.</p>
		 * 
		 * @author Patrick Nicolas
		 * @date March 24, 2014
		 * @project Scala for Machine Learning
		 */
final class WorkerActor extends Actor {
	
		/**
		 * <p>Event loop of the work actor that process two messages<br>
		 * Activate to start processing for the current iteration<br>
		 * Terminate to shutdown.
		 */
   override def receive = {
      case msg: Activate => msg.sender ! Completed(msg.id, variance(msg.data) ) 
      case Terminate => context.stop(self)
      case _ => println("Message not recognized")
   }
   
   private def variance(data: Array[(XY, Int)]): Double = {
  	  val xSeries = data.map( _._1._1 )
  	  val ySeries = data.map( _._1._2 )
  	  Stats[Double](xSeries).variance + Stats[Double](ySeries).variance
   }
}



		/**
		 * <p>Master actor to manage the normalization of groups used in
		 * N-fold cross validation of a supervised machine learning algorithm.</p>
		 * @param workers List of reference to the worker or slave actors.
		 * @param data Data set to be processed by the worker actors
		 * @param numIters Maximum number of iterations allowed to the works to 
		 * @exception IllegalArgumenException if worker actors are undefined, 
		 * the data is undefined or the maximum number of iterations is out of range
		 * 
		 * @author Patrick Nicolas
		 * @date March 24, 2014
		 * @project Scala for Machine Learning
		 */
class MasterActor(val workers: List[ActorRef], val data: XYTSeries, val numIters: Int) extends Actor {
   require(workers != null && workers.size > 0 && workers.size < 32, "Cannot create a master actor with undefined workers")	
   require(data != null && data.size > 0, "Cannot create a master actor to process undefined data")	
   require(numIters > 0 && numIters < 1000, "Number of iteration for data processing in Master " + numIters + " is out of range")
	   
   val normalizer = new GroupsNormalizer(workers.size, data)
   
   		/**
   		 * <p>Event loop of the master actor that processes two messages<br>
   		 * Start to initialize the worker actor and launch the normalization of cross validation groups<br>
   		 * Completed to process the results of the current iteration in the balancing procedure.</p>
   		 */
   override def receive = {
      case msg: Start => execute(0)
      
      case msg: Completed => {
      	 if( normalizer.update(msg.id, msg.variance) ) {
	      	if( msg.id >= numIters) {
	      	   workers.map( _ ! Terminate )
	      	   context.stop(self)
	      	}
	      	else 
	      	   execute(msg.id +1)
	     }
      }
      case _ => println("Message not recognized")
   }
   

   private[this] def execute(id: Int): Unit = 
      workers.zipWithIndex.foreach( w =>  w._1 ! Activate(id, normalizer.groups(w._2), self) )   
}


// ---------------------------------  EOF -------------------------
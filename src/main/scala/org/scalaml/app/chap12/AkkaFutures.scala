/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap12


import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ArrayBuffer
import org.scalaml.core.Types.ScalaMl._
import akka.actor._
import akka.util.Timeout


case class Next(w: XY)


		/**
		 * <p>Base class for the two versions of a future computation of a regression.<br>
		 *  blocking the caller<br>
		 *  Callback the caller.<br>
		 *  The task consists of performing a regression using a gradient function and a timee series
		 *  as input.</p>
		 *  @param data Time series input for which regression has to be computed
		 *  @param gradient gradient function
		 *  @param numFutures number of futures used in the parallelization of the computation
		 *  @param numIters maximum number of iterations used in the regression
		 *  @exception IllegalArgumentException if the class parameters are either undefined or out of range.
		 *  
		 *  @author Patrick Nicolas
		 *  @data March 30, 2014
		 *  @project Scala for Machine Learning
		 */
abstract class RegressionFutures(val data: XYTSeries,
                      			 val gradient: (XY, XY) =>Double, 
                      			 val numFutures: Int,
                      			 val numIters: Int = 200) extends Actor {
	
	require(data != null && data.size > 0, "Cannot execute a regression future with undefined data")
	require(gradient != null , "Cannot execute a regression future with undefined derivatives")
	require( numFutures > 0 && numFutures <32, "Number of futures in Regression " + numFutures + " is out of range")
    require(numIters > 0 && numIters < 1000, "Number of iterations allowed for regression " + numIters + " is out of range")
		
    import scala.util.Random
    
	implicit val timeout = Timeout(2000)
	      
	protected[this] val aggrGradient = new ArrayBuffer[DblVector](numFutures)
	private[this] var counter = numIters
	private[this] var parent: ActorRef = null
	private[this] val partitions = {
       val segSize = data.size/numFutures
	   
	   Range(0, numFutures).foldLeft(List[XYTSeries]())( (xs, n) => {
	  	   val lowerBound = n * segSize
	  	   data.slice( lowerBound, lowerBound + segSize) :: xs
	   }).toArray
    }
	   
	override def preStart: Unit = { println("Setup Actor") }
	    
       /**
        * <pMain co-routine that processes messages
        */
	def receive = {
	  case next: Next => { if( parent == null) parent = sender; iterate(next.w) }
      case _ => println("Not recognized")
	}

	
	override def postStop: Unit = {  }
	
	protected def processHandler(futures: Array[Future[DblVector]], w: XY): Unit
	
	private[this] def iterate(w: XY): Unit = {   
	   if( counter > 0) {
	      val futures = new Array[Future[DblVector]](numFutures)
	      
	      Range(0, numFutures) foreach( i => {
		     futures(i) = Future[DblVector] {
			    partitions(i).map(gradient(_, w) )
			 }
	      }) 
	      counter -= 1
	      processHandler(futures, w)
	   }
	   else 
	      parent ! w
	}
}




		/**
		 * <p>Version of the future regression for which the client or caller blocks until all the
		 * future computations are completed.</p>
		 *  @param _data Time series input for which regression has to be computed
		 *  @param _gradient gradient function
		 *  @param _numFutures number of futures used in the parallelization of the computation
		 *  @param _numIters maximum number of iterations used in the regression
		 *  @exception IllegalArgumentException if the class parameters are either undefined or out of range.
		 *  
		 *  @author Patrick Nicolas
		 *  @data March 30, 2014
		 *  @project Scala for Machine Learning
		 */			
final class RegressionFuturesBlocking(val _data: XYTSeries,
                      				val _gradient: (XY, XY) =>Double, 
                      				val _numFutures: Int,
                      				val _numIters: Int) extends RegressionFutures(_data, _gradient, _numFutures, _numIters) {
  
   override def processHandler(futures: Array[Future[DblVector]], we: XY): Unit = {
       var counter = 1
       
       Range(0, numFutures) foreach( i => {
         val partialGradient = Await.result(futures(i), timeout.duration).asInstanceOf[DblVector]  
         
         if(counter == numFutures) {   
	        val g = aggrGradient.toArray.flatten.reduceLeft( _ + _)
            counter = 1
            self ! Next((-g*we._1, -g*we._2))
	      }
	      counter += 1
	      aggrGradient += partialGradient
       })
   }
}



		/**
		 * <p>Version of the future regression for which the client or caller blocks until all the
		 * future computations are completed.</p>
		 *  @param _data Time series input for which regression has to be computed
		 *  @param _gradient gradient function
		 *  @param _numFutures number of futures used in the parallelization of the computation
		 *  @param _numIters maximum number of iterations used in the regression
		 *  @exception IllegalArgumentException if the class parameters are either undefined or out of range.
		 *  
		 *  @author Patrick Nicolas
		 *  @data March 30, 2014
		 *  @project Scala for Machine Learning
		 */	
final class RegressionFuturesCallback(	val _data: XYTSeries,
                      					val _gradient: (XY, XY) =>Double, 
                      					val _numFutures: Int,
                      					val _numIters: Int) extends RegressionFutures(_data, _gradient, _numFutures, _numIters) {
  
   override def processHandler(futures: Array[Future[DblVector]], we: XY): Unit = {
	  var counter = 1
	  
	  (0 until numFutures) foreach( i => {
	      futures(i) onSuccess {
	        case partialGradient: DblVector => { 
	          if(counter == numFutures) {
	             val g = aggrGradient.flatten.reduceLeft( _ + _)
                 counter = 1
                 self ! Next((-g*we._1, -g*we._2))
	          }
	          else 
		        aggrGradient += partialGradient
		      counter += 1
	        }  
	      }
	      
	      futures(i) onFailure {
	        case e: Exception => Console.println("Failed with future " + i)
	      }
	   })
	}
}


// ------------------------  EOF -----------------------------------------------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap12

import akka.actor.{Actor, ActorSystem, ActorRef, Props}
import akka.util.Timeout
import akka.pattern.ask
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import com.typesafe.config.Config
import scala.io.Source
import org.scalaml.stats.Stats
import org.scalaml.trading.PriceVolume
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import org.scalaml.workflow.data.DataSource
import org.scalaml.core.Types
import Types.ScalaMl._

case class Next(w: XY)


		/**
		 * Create a task that trigger a another task/actor to simulate future
		 */
abstract class RegressionFutures(val data: XYTSeries,
                      			 val derivatives: (XY, XY) =>Double, 
                      			 val numFutures: Int,
                      			 val maxNumIters: Int = 200) extends Actor {
	
	require(data != null && data.size > 0, "Cannot execute a regression future with undefined data")
	require(derivatives != null , "Cannot execute a regression future with undefined derivatives")
	require( numFutures > 0 && numFutures <32, "Number of futures in Regression " + numFutures + " is out of range")
    require(  maxNumIters > 0 && maxNumIters < 1000, "Number of iterations allowed for regression " + maxNumIters + " is out of range")
		
    import scala.util.Random
    
	implicit val timeout = Timeout(2000)
	      
	protected[this] val aggrGradient = new ArrayBuffer[DblVector](numFutures)
	private[this] var numIters = maxNumIters
	
	private[this] val partitions = {
       val partitionsBuffer = new ArrayBuffer[XYTSeries]
       val segSize = data.size/numFutures
	   var index = 0
	   Range(0, numFutures).foreach( _ => {
	       partitionsBuffer.append( data.slice(index, index + segSize) )
	       index += segSize
	   })
	   partitionsBuffer.toArray
    }
	   
	override def preStart: Unit = { println("Setup Actor") }
	     
    private[this] var parent: ActorRef = null
    
	def receive = {
	  case next: Next => { if( parent == null) parent = sender; iterate(next.w) }
      case _ => println("Not recognized")
	}

	
	override def postStop: Unit = {  }
	
	protected def processHandler(futures: Array[Future[DblVector]], w: XY): Unit
	
	
	private[this] def iterate(w: XY): Unit = {   
	   if( numIters > 0) {
	      val futures = new Array[Future[DblVector]](numFutures)
	      
	      Range(0, numFutures) foreach( i => {
		     futures(i) = Future[DblVector] {
			    partitions(i).map(derivatives(_, w) )
			 }
	      }) 
	      numIters -= 1
	      processHandler(futures, w)
	   }
	   else 
	      parent ! w
	}
}


final class RegressionFuturesBlocking(val _data: XYTSeries,
                      				val _derivatives: (XY, XY) =>Double, 
                      				val _numFutures: Int,
                      				val _maxNumIters: Int) extends RegressionFutures(_data, _derivatives, _numFutures, _maxNumIters) {
  
   override def processHandler(futures: Array[Future[DblVector]], we: XY): Unit = {
       var counter = 1
       
       (0 until numFutures) foreach( i => {
         val partialGradient = Await.result(futures(i), timeout.duration).asInstanceOf[DblVector]  
         
         if(counter == numFutures) {   
	        val g = aggrGradient.toArray.flatten.reduceLeft( _ + _)
            counter = 1
            println("Apply to next")
            self ! Next((-g*we._1, -g*we._2))
	      }
	      counter += 1
	      aggrGradient += partialGradient
     
       })
   }
}



final class RegressionFuturesCallback(	val _data: XYTSeries,
                      					val _derivatives: (XY, XY) =>Double, 
                      					val _numFutures: Int,
                      					val _maxNumIters: Int) extends RegressionFutures(_data, _derivatives, _numFutures, _maxNumIters) {
  
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


object AkkaFuturesEval {
   import scala.util.Random   
   import org.scalaml.workflow.data.DataSource
   
   implicit val timeout = Timeout(20000)
  	   
   val logItGradient = (x: XY, w: XY) => {
       val exponent = - x._2*(w._1*x._1 + w._2)
       (1.0/(1.0 + Math.exp(-exponent)) -1.0)*x._1*x._2
   }
      
   
   val transforms = List[Array[String] => Double](
  	 (f:Array[String]) => f(PriceVolume.HIGH.id).toDouble - f(PriceVolume.LOW.id).toDouble,
  	  (f:Array[String]) => f(PriceVolume.VOLUME.id).toDouble
   )

   val dataSource = DataSource("resources/data/chap12/CSCO.csv",  true)
   dataSource |> transforms match {
     case Some(volatilityVol) => {
       val volatility_volume: XYTSeries = volatilityVol(0).arr.zip(volatilityVol(1).arr)
  
       implicit val actorSystem = ActorSystem("system")
      
       val callback = new RegressionFuturesCallback( volatility_volume, logItGradient, 6, 10)
       val regression = actorSystem.actorOf(Props(callback), name = "Regressionfuture")
    
       val fut = regression ? Next((Random.nextDouble, Random.nextDouble))
       val results = Await.result(fut, timeout.duration).asInstanceOf[XY]
       println(results.toString)
       actorSystem.shutdown
     }
     
     case None => Console.println("Cannot load volatility and volume data")
   }
}



// ------------------------  EOF -----------------------------------------------------------------------------
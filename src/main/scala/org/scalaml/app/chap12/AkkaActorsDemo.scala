/**
 * Classes that illustrates the use of Futures in Scala 2.10.2 and Akka 2.14 framework. The pattern consists of a 
 * main task, 'MainTask' that launches than blocks on a future task 'FutureTask' 
 * This code is provided solely for the purpose of illustrate concepts introduced or described 
 * in some of the  of the post in the blog
 * 
 * @author Patrick Nicolas
 * @date July 23, 2013
 * 
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
                      			 val maxNumIters: Int = 100) extends Actor {
    import scala.util.Random
    
	implicit val timeout = Timeout(2000)
	      
	protected[this] val aggrGradient = new ArrayBuffer[Array[Double]](numFutures)
	private[this] var numIters = maxNumIters
	
	private[this] val partitions = {
       val partitionsBuffer = new ArrayBuffer[XYTSeries]
       val segSize = data.size/numFutures
	   var index = 0
	   (0 until numFutures) foreach( i => {
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
	      
	      (0 until numFutures) foreach( i => {
		     futures(i) = Future[Array[Double]] {
			    partitions(i).map(x => derivatives(x, w) )
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


object FutureApp extends App {
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
      
       val regression = actorSystem.actorOf(Props(classOf[RegressionFuturesCallback], 
    									      volatility_volume, 
    									      logItGradient, 
    									      6, 
    									      10), name = "Regressionfuture")
    
       val rGen =  new Random(System.currentTimeMillis)
       val fut = regression ? Next((rGen.nextDouble, rGen.nextDouble))
       val results = Await.result(fut, timeout.duration).asInstanceOf[XY]
       println(results.toString)
       actorSystem.shutdown
     }
     
     case None => Console.println("Cannot load volatility and volume data")
   }
}



// ------------------------  EOF -----------------------------------------------------------------------------
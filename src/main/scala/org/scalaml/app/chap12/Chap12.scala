package org.scalaml.app.chap12

import scala.collection.parallel.mutable.ParArray
import scala.collection.mutable.ArraySeq
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import scala.actors.Actor
import org.scalaml.core.XTSeries




object Chap12 extends App {
   import org.scalaml.core.Types.ScalaMl._
   
   private[this] def extractVolatilityVolume: Option[List[XTSeries[Double]]] = {
      import org.scalaml.trading.PriceVolume
      import org.scalaml.workflow.data.DataSource
        
      val extractors = List[Array[String] => Double](
	  	 (f:Array[String]) => f(PriceVolume.HIGH.id).toDouble - f(PriceVolume.LOW.id).toDouble,
	  	 (f:Array[String]) => f(PriceVolume.VOLUME.id).toDouble
	  )		
	  DataSource("resources/data/chap12/CSCO.csv", true) |> extractors
   }
   
   val logItGradient = (x: XY, w: XY) => {
	  val exponent = - x._2*(w._1*x._1 + w._2)
	  (1.0/(1.0 + Math.exp(-exponent)) -1.0)*x._1*x._2
   }
	
   final val numWorkers = 5
   final val numIters = 100
   
      
   def testFutures = {
      import scala.util.Random   
      import scala.io.Source
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.{Await, Future}
      
      import akka.pattern.ask
      import akka.actor.{Actor, ActorSystem, ActorRef, Props}
	  import akka.util.Timeout

	  import com.typesafe.config.Config


      implicit val timeout = Timeout(20000)
	  		
	  extractVolatilityVolume match {
		  case Some(volatilityVol) => {
			 val volatility_volume = volatilityVol(0).arr.zip(volatilityVol(1).arr)
			  
			 implicit val actorSystem = ActorSystem("system")
			      
			 val regression = actorSystem.actorOf(Props(classOf[RegressionFuturesCallback], 
			    									    volatility_volume, 
			    									    logItGradient, 
			    									    numWorkers, 
			    									    numIters), name = "Regressionfuture")
			    
			 val rGen =  new Random(System.currentTimeMillis)
			 val fut = regression ? Next((rGen.nextDouble, rGen.nextDouble))
		
			 val results = Await.result(fut, timeout.duration).asInstanceOf[XY]
			 println(results.toString)
			 actorSystem.shutdown
		  }
		  case None => Console.println("Cannot extract volatility and volume data")
	  }
    }
   
   
   def testScalaActors: Unit = {
	  import scala.io.Source
	  import org.scalaml.trading._
	  import scala.concurrent.duration.Duration
	  import scala.concurrent.{Await, Awaitable}
	   
	  val volatilityVol = extractVolatilityVolume
	  extractVolatilityVolume match {
		  case Some(volatilityVol) => {
		     val volatility_volume = volatilityVol(0).arr.zip(volatilityVol(1).arr)
		   
		     val workers = List.fill(numWorkers)(new WorkerActor(logItGradient))
		     val master = new MasterActor(workers, volatility_volume, numIters)
		     master.start
		     master ! Start
		  }
		  
		  case none =>  Console.println("Cannot extract volatility and volume data")
	  }
   }

   def testParallelCollections: Unit  = {
	  val sz = 100000
	  val data = Array.tabulate(sz) ( _ << 1)
		
	  data.par.map( _ >> 1)
	  data.par.reduceLeft( _ + _)
	
	  for( i <- 100000 to 2000000 by 100000) {
	     val data = Array.tabulate(i) ( _ * 0.1)
		 val test = new ParallelCollections[Double](data, 4)
			
		 test.map(x => 2.5*Math.sin(0.01*x) + 2.0*Math.cos(0.01*x))
		 test.reduceLeft((s, x) => s + x)
	  }
   }
}


// ---------------------------------  EOF -------------------------
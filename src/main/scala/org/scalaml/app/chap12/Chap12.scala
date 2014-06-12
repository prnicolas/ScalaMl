/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap12


import org.scalaml.core.XTSeries
import scala.util.Random
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import com.typesafe.config.Config
import akka.actor.Props
import scala.concurrent.Await
import org.scalaml.core.Types.ScalaMl._


object ScalaParallelCollectionEval {
  final val descriptor = "Evaluation performance Scala parallel collections"	
	
  def run(args: Array[String] = null)  {
     import scala.collection.parallel.mutable.ParArray
     import scala.collection.mutable.ArraySeq
     import scala.collection.parallel.ForkJoinTaskSupport
     import scala.concurrent.forkjoin.ForkJoinPool
     
     Console.println(descriptor)

	 val sz = 100000
	 val data = Array.tabulate(sz) ( _ << 1)
		
	 data.par.map( _ >> 1)
	 data.par.reduceLeft( _ + _)
	
	 (100000 to 2000000 by 100000) foreach( i => {
	    val data = Array.tabulate( i) ( _ * 0.1)
	    val test = new ParallelCollections[Double](data, 4)
			
		test.map(x => 2.5*Math.sin(0.01*x) + 2.0*Math.cos(0.01*x))
		test.reduceLeft((s, x) => s + x)
	 })
  }
}


trait AkkaEval {
   final val numWorkers = 4
   final val numIters = 6

   protected def extractVolatilityVolume: Option[List[XTSeries[Double]]] = {
      import org.scalaml.trading.PriceVolume
      import org.scalaml.workflow.data.DataSource
        
      val extractors = List[Array[String] => Double](
	  	 (f:Array[String]) => f(PriceVolume.HIGH.id).toDouble - f(PriceVolume.LOW.id).toDouble,
	  	 (f:Array[String]) => f(PriceVolume.VOLUME.id).toDouble )		
	  DataSource("resources/data/chap12/CSCO.csv", true) |> extractors
   }
}



object AkkaActorEval extends AkkaEval {
   final val descriptor = "Evaluation Akka actors"	
    	 
   def run(args: Array[String] = null): Unit = {
	  import org.scalaml.trading._
	  import scala.concurrent.duration.Duration
	  import scala.concurrent.{Await, Awaitable}
	  import akka.actor.ActorSystem	 
	  
	  implicit val actorSystem = ActorSystem("system") 
	  
	  Console.println(descriptor)
	  extractVolatilityVolume match {
		  case Some(x) => {
		     val volatilityVol = x(0).arr.zip(x(1).arr)
		     
		     val workers = List.tabulate(numWorkers)(n => 
		    	    actorSystem.actorOf(Props(new WorkerActor), name = "worker" + String.valueOf(n)))
		     
		     val master = actorSystem.actorOf(Props(new MasterActor(workers, volatilityVol, numIters)), "Master")
		     master ! Start(0)
		  }
		  
		  case none =>  Console.println("Cannot extract volatility and volume data")
	  }
   }
}


object AkkaFutureEval extends AkkaEval {
    final val descriptor = "Evaluation Akka futures"	
	      	
	def run(args: Array[String] = null) = {
  	  import akka.actor.ActorSystem
  	  import akka.util.Timeout
  	  import akka.pattern.ask
  	  
      implicit val timeout = Timeout(20000)
	  		
      Console.println(descriptor)
	  extractVolatilityVolume match {
		  case Some(volatilityVol) => {
			 val volatility_volume = volatilityVol(0).arr.zip(volatilityVol(1).arr)
			  
			 implicit val actorSystem = ActorSystem("system")
			      
             val regression = actorSystem.actorOf(
            		              Props(new FoldNormalizerBlocking(volatility_volume, numWorkers)), 
            		                    name = "Regressionfuture")

			 val fut = regression ? Launch
		
			 val results = Await.result(fut, timeout.duration).asInstanceOf[Double]
			 println(results.toString)
			 actorSystem.shutdown
		  }
		  case None => Console.println("Cannot extract volatility and volume data")
	  }
    }
}




object Chap12 extends App {
	import java.util.concurrent.TimeoutException
	try {
//   ScalaParallelCollectionEval.run()
      AkkaActorEval.run()
	 //   AkkaFutureEval.run()
	}
	catch {
		case e: TimeoutException  => println("Execution time out " + e.toString)
		case e: RuntimeException =>  println("Runtime error " + e.toString); e.printStackTrace
	}
}


// ---------------------------------  EOF -------------------------
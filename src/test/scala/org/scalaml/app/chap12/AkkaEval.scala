/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95
 */
package org.scalaml.app.chap12


import org.scalaml.core.XTSeries
import scala.util.Random
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import com.typesafe.config.Config
import akka.actor.Props
import scala.concurrent.{Await, duration}
import org.scalaml.core.types.ScalaMl._
import org.scalaml.scalability.spark._
import org.apache.spark.SparkContext
import org.apache.spark.storage.StorageLevel
import org.scalaml.scalability.akka._
import org.scalaml.scalability.scala._


trait AkkaEval {
   final val numWorkers = 4
   final val numIters = 6

   protected def extractVolatilityVolume: List[DblVector] = {
      import org.scalaml.trading.YahooFinancials
      import org.scalaml.workflow.data.DataSource
        
      val extractors = List[Array[String] => Double](
      	 YahooFinancials.volatility, YahooFinancials.volume )	
      	 
	  DataSource("resources/data/chap12/CSCO.csv", true) |> extractors
   }
}


/*

object AkkaActorEval extends AkkaEval {
   final val descriptor = "Evaluation Akka actors"	
    	 
   def run: Unit = {
	  import org.scalaml.trading._
	  import scala.concurrent.duration.Duration
	  import scala.concurrent.{Await, Awaitable}
	  import akka.actor.ActorSystem	 
	  
	  implicit val actorSystem = ActorSystem("system") 
	  
	  Console.println(descriptor)
	  extractVolatilityVolume match {
		  case Some(x) => {
		     val volatilityVol = x(0).zip(x(1)).toArray
		     
		     val workers = List.tabulate(numWorkers)(n => 
		    	    actorSystem.actorOf(Props(new WorkerActor), name = "worker" + String.valueOf(n)))
		     
		     val master = actorSystem.actorOf(Props(new MasterActor(workers, volatilityVol, numIters)), "Master")
		     master ! Start(0)
		     actorSystem.awaitTermination(Duration(5000, "millis"))
		     actorSystem.shutdown
		  }
		  
		  case none =>  Console.println("Cannot extract volatility and volume data")
	  }
   }
}


object AkkaFutureEval extends AkkaEval {
    final val descriptor = "Evaluation Akka futures"	
	      	
	def run: Unit = {
  	  import akka.actor.ActorSystem
  	  import akka.util.Timeout
  	  import akka.pattern.ask
  	  
      implicit val timeout = Timeout(20000)
	  		
      Console.println(descriptor)
	  extractVolatilityVolume match {
		  case Some(x) => {
			 val volatilityVol = x(0).zip(x(1)).toArray
			  
			 implicit val actorSystem = ActorSystem("system")
			 val normalizer = new GroupsNormalizerBlocking(volatilityVol, numWorkers)
             val normalization = actorSystem.actorOf(Props(normalizer), name = "Regressionfuture")

			 val fut = normalization ? Launch
		
			 val results = Await.result(fut, timeout.duration).asInstanceOf[Double]
			 actorSystem.shutdown
		  }
		  case None => Console.println("Cannot extract volatility and volume data")
	  }
    }
}
* 
*/


// ---------------------------------  EOF -------------------------
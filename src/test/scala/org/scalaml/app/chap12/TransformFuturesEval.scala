/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95d
 */
package org.scalaml.app.chap12

import scala.collection.GenTraversableOnce
import akka.pattern.ask
import org.scalaml.app.Eval
import org.apache.log4j.Logger
import org.scalaml.core.XTSeries
import org.scalaml.scalability.akka.Partitioner
import akka.actor.ActorSystem
import scala.concurrent.Await
import akka.actor.Props
import scala.util.{Random, Try, Success, Failure}
import org.scalaml.scalability.akka.TransformFutures
import scala.concurrent.duration._
import akka.util.Timeout
import org.scalaml.filtering.DFT
import org.scalaml.scalability.akka.Start
import org.scalaml.util.Display
import java.util.concurrent.TimeoutException



class DFTTransformFutures(xt: XTSeries[Double], partitioner: Partitioner)(implicit timeout: Timeout) 
				extends TransformFutures(xt, DFT[Double], partitioner)  {
  
	override protected def aggregate(freqs: Array[XTSeries[Double]]): Seq[Double] = {
		freqs.map(_.toArray).transpose.map(_.sum).take(10).toSeq
	}
}


object TransformFuturesEval extends Eval {
	val name: String = "TransformFuturesEval"
	private val logger = Logger.getLogger(name)
		
	val NUM_WORKERS = 8
	val NUM_DATA_POINTS = 1000000
	val h = (x:Double) =>	2.0*Math.cos(Math.PI*0.005*x) +  // simulated first harmonic
							Math.cos(Math.PI*0.05*x) +   // simulated second harmonic
							0.5*Math.cos(Math.PI*0.2*x)  +    // simulated third harmonic 
							0.2*Random.nextDouble

	val duration = Duration(10000, "millis")
	implicit val timeout = new Timeout(duration)
	implicit val actorSystem = ActorSystem("system") 
     
	def run(args: Array[String]): Int = {
		Display.show(s"$name evaluation of Akka futures", logger)
		
		val xt = XTSeries[Double](Array.tabulate(NUM_DATA_POINTS)(h(_)))
		val partitioner = new Partitioner(NUM_WORKERS)
  
		val master = actorSystem.actorOf(Props(new DFTTransformFutures(xt, partitioner)), "DFTTransform")
		Try {
			val future = master ? Start
			val result = Await.result(future, timeout.duration)
			actorSystem.shutdown
			result
		} match {
			case Success(result) => Display.show(s"TransformFuturesEval.run: ${result.toString}", logger)
			case Failure(e) => e match {
				case ex: TimeoutException => Display.show("TransformFuturesEval.run completed", logger)
				case ex: Throwable => Display.error("TransformFuturesEval.run completed", logger, e)
			}
		}
	}
}


object TransformFuturesEvalApp extends App {
	TransformFuturesEval.run(Array.empty)
}
// -----------------------------------------------  EOF ---------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95c
 */
package org.scalaml.app.chap12

import akka.pattern.ask
import org.scalaml.app.Eval
import org.apache.log4j.Logger
import org.scalaml.core.XTSeries
import org.scalaml.scalability.akka.Partitioner
import akka.actor.ActorSystem
import scala.concurrent.Await
import akka.actor.Props
import scala.util.Random
import org.scalaml.scalability.akka.TransformFutures
import scala.concurrent.duration.Duration
import akka.util.Timeout
import org.scalaml.filtering.DFT
import org.scalaml.scalability.akka.Start
import org.scalaml.util.Display


object TransformFuturesEval extends Eval {
	val name: String = "TransformFuturesEval"
		
    val NUM_WORKERS = 8
    val NUM_DATA_POINTS = 1000000
	private val logger = Logger.getLogger(name)
	
	val h = (x:Double) =>2.0*Math.cos(Math.PI*0.005*x) +  // simulated first harmonic
                            Math.cos(Math.PI*0.05*x) +   // simulated second harmonic
                        0.5*Math.cos(Math.PI*0.2*x)  +    // simulated third harmonic 
                        0.2*Random.nextDouble
	
    implicit val timeout = Timeout(20000)
    
	def run(args: Array[String]): Int = {
		Display.show(s"$name evaluation of Akka futures", logger)
		
		val xt = XTSeries[Double](Array.tabulate(NUM_DATA_POINTS)(h(_)))
        val partitioner = new Partitioner(NUM_WORKERS)
   
        implicit val actorSystem = ActorSystem("system") 

        val master = actorSystem.actorOf(Props(new TransformFutures(xt, DFT[Double], partitioner)), "Transform") 
        val future = master ? Start
        Await.result(future, timeout.duration)
        actorSystem.shutdown
        0
	}
}

// -----------------------------------------------  EOF ---------------------------
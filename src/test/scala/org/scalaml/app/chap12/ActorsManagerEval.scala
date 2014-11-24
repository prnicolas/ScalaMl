/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.app.chap12

import org.scalaml.app.Eval
import scala.util.Random
import org.scalaml.scalability.akka.Partitioner
import org.scalaml.scalability.akka.message._
import org.scalaml.core.XTSeries
import akka.actor.Props
import org.scalaml.scalability.akka.Master
import org.scalaml.filtering.DFT
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Awaitable}
import akka.actor.ActorSystem
import org.apache.log4j.Logger
import org.scalaml.core.types.ScalaMl._
import XTSeries._
import org.scalaml.util.Display


		/**
		 * <p>Specialized Akka master actor for the distributed discrete Fourier transform.</p>
		 * @constructor Create a master actor for the distributed discrete Fourier transform. [xt] time series to be processed. [partitioner] Partitioning methodology for distributing time series across a cluster of worker actors.
		 * @throws IllegalArgumentException if the time series or the partitioner are not defined.
		 * 
		 * @author Patrick Nicolas
		 * @since June 5, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks/Akka
		 */
class DFTMaster(xt: DblSeries, partitioner: Partitioner) extends Master(xt, DFT[Double], partitioner) {
	
		/**
		 * <p>Aggregation of the results for the discrete Fourier transform for each worker actor.</p>
		 * @return Sequence of frequencies 
		 */
	override protected def aggregate: Seq[Double] = 
	  aggregator.transpose.map( _.sum).toSeq
}



object ActorsManagerEval extends Eval {   
	val name: String = "ActorsManagerEval"
	private val logger = Logger.getLogger(name)
	
	val DONE= 0
	val NUM_WORKERS = 4
	val NUM_DATA_POINTS = 1000000
	val h = (x:Double) =>	2.0*Math.cos(Math.PI*0.005*x) +	// simulated first harmonic
							Math.cos(Math.PI*0.05*x) +   	// simulated second harmonic
							0.5*Math.cos(Math.PI*0.2*x) + 	// simulated third harmonic 
							0.2*Random.nextDouble			// noise
	 
	implicit val actorSystem = ActorSystem("system") 
	
		/** <p>Execution of the scalatest for Master-worker design with Akka framework.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		Display.show(s"\n** test#${Eval.testCount} $name Master-Worker model for Akka actors", logger)
		val xt = XTSeries[Double](Array.tabulate(NUM_DATA_POINTS)(h(_)))
		val partitioner = new Partitioner(NUM_WORKERS)
	
		val master = actorSystem.actorOf(Props(new DFTMaster(xt, partitioner)), "Master")
		master ! Start
		Thread.sleep(5000)
		actorSystem.shutdown
		DONE
	}
}


// ----------------------------------  EOF ------------------------
/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.app.chap12

import scala.util.Random
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Awaitable}
import scala.collection._
import org.apache.log4j.Logger
import akka.actor.{Props, ActorSystem}

import org.scalaml.app.Eval
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.scalability.akka.{Partitioner, Master, MasterWithRouter, Controller}
import org.scalaml.scalability.akka.message.Start
import org.scalaml.filtering.DFT
import org.scalaml.util.DisplayUtils
import XTSeries.DblSeries


		/**
		 * <p>Specialized Akka master actor for the distributed discrete Fourier transform without 
		 * routing.</p>
		 * @constructor Create a master actor for the distributed discrete Fourier transform. 
		 * @throws IllegalArgumentException if the time series or the partitioner are not defined.
		 * @param xt Time series to be processed
		 * @param partitioner Methodology to partition a time series in segments or partitions to be 
		 * processed by workers
		 * @param aggrFreq User defined aggregation and monitoring method
		 * 
		 * @author Patrick Nicolas
		 * @since June 5, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks / Akka
		 */
protected class DFTMaster(
		xt: DblSeries, 
		partitioner: Partitioner, 
		aggrFreq: List[DblVector]=>immutable.Seq[Double]) 
				extends Master(xt, DFT[Double], partitioner, aggrFreq)


		/**
		 * <p>Specialized Akka master actor for the distributed discrete Fourier transform routing.</p>
		 * @constructor Create a master actor for the distributed discrete Fourier transform. 
		 * @throws IllegalArgumentException if the time series or the partitioner are not defined.
		 * @param xt Time series to be processed
		 * @param partitioner Methodology to partition a time series in segments or partitions to be 
		 * processed by workers
		 * @param aggrFreq User defined aggregation and monitoring method
		 * 
		 * @author Patrick Nicolas
		 * @since June 5, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks / Akka
		 */
protected class DFTMasterWithRouter(
		xt: DblSeries, 
		partitioner: Partitioner, 
		aggrFreq: List[DblVector] => immutable.Seq[Double]) 
				extends MasterWithRouter(xt, DFT[Double], partitioner, aggrFreq) 


		/**
		 * <p><b>Purpose</b>: Singleton to understand the behavior of Master-worker
		 * design with Akka actors</p>
		 * 
		 * @author Patrick Nicolas 
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks/Scala/Akka actors
		 */
object ActorsManagerEval extends Eval { 	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "ActorsManagerEval"
	
	val NUM_WORKERS = 4
	val NUM_DATA_POINTS = 1000000
		
		// Synthetic generation function for multi-frequencies signals
	val h = (x:Double) =>	2.0*Math.cos(Math.PI*0.005*x) +	// simulated first harmonic
							Math.cos(Math.PI*0.05*x) +   	// simulated second harmonic
							0.5*Math.cos(Math.PI*0.2*x) + 	// simulated third harmonic 
							0.2*Random.nextDouble					// noise
	
		/** 
		 * <p>Execution of the scalatest for Master-worker design with Akka framework.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Master-Worker model for Akka actors with ${args(0)}", logger)
		val actorSystem = ActorSystem("System") 
		
		if(args.size > 0) {
			val xt = XTSeries[Double](Array.tabulate(NUM_DATA_POINTS)(h(_)))
			val partitioner = new Partitioner(NUM_WORKERS)
			
			val displaySize = if( args(0) == "router" ) 64 else 256
		
				// User defined method that define the aggregation of the results 
				// for the discrete Fourier transform for each worker actor
			def aggrFrequencies(aggrBuffer: List[DblVector]): immutable.Seq[Double] = {
			  
				def display(x: DblVector): Unit = {
					import org.scalaml.plots.{LinePlot, LightPlotTheme}
					val labels = List[String](
						name,
						"Distributed DFT- Akka",
						s"Frequencies distribution with ${args(0)}",
						"frequencies"
					)
					
					LinePlot.display(x, labels, new LightPlotTheme)
				}
				
				// Aggregate the results by transposing the observations
				// and sum the value for each dimension...
				val results = aggrBuffer.transpose.map( _.sum).toSeq
				DisplayUtils.show(s"DFT display ${results.size} frequencies", logger)
				display(results.toArray)
				results
			}
			
				// The argument specifies if the group of worker actors is supervised
				// by a routing actor or not..
			val controller = if(args(0) == "router")
				actorSystem.actorOf(Props(new DFTMasterWithRouter(xt, partitioner, 
						aggrFrequencies)),	"MasterWithRouter")
			else
				actorSystem.actorOf(Props(new DFTMaster(xt, partitioner, aggrFrequencies)), "Master")
		
				// Launch the execution
			controller ! Start(1)
			DisplayUtils.show(s"$name.run completed", logger)
		}
		else
			DisplayUtils.error(s"$name Master-Worker model, arguments undefined", logger)
	}
}


object MyApp extends App {
  ActorsManagerEval.run(Array[String]("router"))
}
// ----------------------------------  EOF ------------------------
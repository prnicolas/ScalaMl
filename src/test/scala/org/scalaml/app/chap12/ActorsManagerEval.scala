/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99
 */
package org.scalaml.app.chap12

import scala.util.{Try, Random}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Awaitable}
import scala.collection._
import org.apache.log4j.Logger
import akka.actor.{Props, ActorSystem}

import org.scalaml.app.Eval
import org.scalaml.stats.XTSeries
import org.scalaml.core.Types.ScalaMl.{DblArray, DblVector}
import org.scalaml.scalability.akka.{Master, MasterWithRouter, Controller}
import org.scalaml.scalability.akka.message.Start
import org.scalaml.filtering.dft.DFT
import DFTMaster._


		/**
		 * Specialized Akka master actor for the distributed discrete Fourier transform without 
		 * routing.
		 * @constructor Create a master actor for the distributed discrete Fourier transform. 
		 * @throws IllegalArgumentException if the time series or the partitioner are not defined.
		 * @param xt Time series to be processed
		 * @param partitioner Methodology to partition a time series in segments or partitions to be 
		 * processed by workers
		 * @param aggrFreq User defined aggregation and monitoring method
		 * 
		 * @author Patrick Nicolas
		 * @since June 5, 2014
		 * @see Scala for Machine Learning Chapter 12 Scalable frameworks / Akka
		 */
protected class DFTMaster(
		xt: DblVector, 
		nPartitions: Int, 
		reducer: Reducer) 
				extends Master(xt, DFT[Double].|>, nPartitions)


		/**
		 * Specialized Akka master actor for the distributed discrete Fourier transform routing.
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
		xt: DblVector, 
		nPartitions: Int, 
		reducer: Reducer) 
				extends MasterWithRouter(xt, DFT[Double].|>, nPartitions) 


object DFTMaster {
	type Reducer = List[DblVector] => immutable.Seq[Double]
}

		/**
		 * '''Purpose''': Singleton to understand the behavior of Master-worker
		 * design with Akka actors
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
		 * Execution of the scalatest for Master-worker design with Akka framework.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate
		 * 	
		 * Exceptions thrown during the execution of the tests are caught by the wrapper or handler
		 * test method in Eval trait defined as follows:
		 * {{{
		 *    def test(args: Array[String]) =
		 *      Try(run(args)) match {
		 *        case Success(n) => ...
		 *        case Failure(e) => ...
		 * }}}
		 * The tests can be executed through ''sbt run'' or individually by calling 
		 * ''TestName.test(args)'' (i.e. DKalmanEval.test(Array[String]("IBM") )
		 * @param args array of arguments used in the test 
		 */
	override protected def run(args: Array[String]): Int = {
		show(s"$header Master-Worker model for Akka actors with ${args(0)}")
		val actorSystem = ActorSystem("System") 
		
		if(args.length > 0) {
			val xt = Vector.tabulate(NUM_DATA_POINTS)(h(_))
			
			val displaySize = if( args(0) == "router" ) 64 else 256
		
				// User defined method that define the aggregation of the results 
				// for the discrete Fourier transform for each worker actor
			def fReduce(aggrBuffer: List[DblVector]): immutable.Seq[Double] = {
			  
				def display(x: DblArray): Unit = {
					import org.scalaml.plots.{LinePlot, LightPlotTheme, Legend}
					val labels = Legend(
						name,
						"Distributed DFT- Akka",
						s"DFT-Akka Frequencies distribution for ${args(0)}",
						"frequencies"
					)
					
					LinePlot.display(x.toVector, labels, new LightPlotTheme)
				}
				
				// Aggregate the results by transposing the observations
				// and sum the value for each dimension...
				val results = aggrBuffer.transpose.map( _.sum).toSeq
				show(s"DFT display ${results.size} frequencies")
				display(results.toArray)
				results
			}
			
				// The argument specifies if the group of worker actors is supervised
				// by a routing actor or not..
			val controller = if(args(0) == "router")
				actorSystem.actorOf(Props(new DFTMasterWithRouter(xt, NUM_WORKERS, fReduce)),	"MasterWithRouter")
			else
				actorSystem.actorOf(Props(new DFTMaster(xt, NUM_WORKERS, fReduce)), "Master")
		
				// Launch the execution
			controller ! Start(1)
			1
		}
		else
			error(s"$name Master-Worker model, arguments undefined")
	}
}

// ----------------------------------  EOF ------------------------
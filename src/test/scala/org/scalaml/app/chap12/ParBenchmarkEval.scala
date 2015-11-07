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

import org.scalaml.scalability.scala.{ParArrayBenchmark, ParMapBenchmark}
import org.scalaml.app.Eval
import org.scalaml.util.{DisplayUtils, LoggingUtils}
import LoggingUtils._
import org.scalaml.core.Types.ScalaMl.DblArray

	/**
	 * '''Purpose''': Singleton to evaluate the performance of Scala parallel arrays
	 * and maps.
	 * 
	 * @author Patrick Nicolas 
	 * @note Scala for Machine Learning Chapter 12 Scalable frameworks/Scala/Parallel collections
	 */
object ParBenchmarkEval extends Eval {
	import scala.util.{Random, Try, Success, Failure}
	import scala.collection.mutable.HashMap
	import scala.collection.parallel.immutable.ParVector
	import scala.collection.parallel.mutable.{ParArray, ParHashMap}
	import scala.collection.parallel.ForkJoinTaskSupport
	import scala.concurrent.forkjoin.ForkJoinPool
	import org.apache.log4j.Logger
	 
	 	/**
		 * Name of the evaluation 
		 */
	val name: String = "ParBenchmarkeval"

	private val SZ = 100000
	private val NUM_TASKS = 8
	private val evalRange = Range(1, NUM_TASKS)
	private val TIMES = 20

	 	/** Execution of the scalatest for Master-worker design with Akka framework.
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
		show(s"$header Scala parallel collections")
		 
		if( args.size > 0) {
			// Arbitrary map function
			val mapF = (x: Double) => Math.sin(x*0.01) + Math.exp(-x)
				// Arbitrary filter function
			val filterF = (x: Double) => (x > 0.8)
			 	// Arbitrary reduce function
			val reduceF = (x:Double, y:Double) => (x+y)*x
			 
			show(s"$name Comparative benchmark for $NUM_TASKS tasks\nIter\tRatio")
			
			if(args(0) == "array")
				evaluateParArray(mapF, filterF, reduceF)
			else
				evaluateParMap(mapF, filterF)
			1
		}
		else 
			error(s"$name Incorrect cmd line argument")
	}
	 
	private def evaluateParArray(
			mapF: Double => Double, 
			filterF: Double => Boolean, 
			reduceF: (Double, Double) => Double): Unit =  {
	  
			// Generate random vector for both the non-parallel and parallel array
		 val data = Array.fill(SZ)(Random.nextDouble)
		 val pData = ParArray.fill(SZ)(Random.nextDouble)
	
		 	// Initialized and execute the benchmark for the parallel array
		 val benchmark = new ParArrayBenchmark[Double](data, pData, TIMES)
		 
		 show("Mapper for (x: Double) => Math.sin(x*0.01) + Math.exp(-x)")
		 val ratios = new Array[Double](NUM_TASKS)
		 evalRange.foreach(n => ratios.update(n, benchmark.map(mapF)(n)))
		 display(ratios.drop(1), "ParArray.map")
		 
		 show("Filter for (x: Double) => (x > 0.8)")
		 evalRange.foreach(n => ratios.update(n, benchmark.filter(filterF)(n)))
		 display(ratios.drop(1), "ParArray.filter")
	}
	
	private def evaluateParMap(mapF: Double => Double, filterF: Double => Boolean): Unit = {
		 val mapData = new HashMap[Int, Double]
		 Range(0, SZ).foreach(n => mapData.put(n, Random.nextDouble) )
		 val parMapData = new ParHashMap[Int, Double]
		 Range(0, SZ).foreach(n => parMapData.put(n, Random.nextDouble) )
		 
		  	// Initialized and execute the benchmark for the parallel map
		 val benchmark = new ParMapBenchmark[Double](mapData.toMap, parMapData, TIMES)
		 show("Mapper for (x: Double) => Math.sin(x*0.01) + Math.exp(-x)")
	
		 val ratios = new Array[Double](NUM_TASKS)
		 evalRange.foreach(n => ratios.update(n, benchmark.map(mapF)(n)))
		 display(ratios.drop(1), "ParMap.map")
		 
		 show("Filter for (x: Double) => (x > 0.8)")
		 evalRange.foreach(n => ratios.update(n, benchmark.filter(filterF)(n)))
		 display(ratios.drop(1), "ParMap.filter")
	}
	
	
	private def display(x: DblArray, label: String): Unit =   {
		import org.scalaml.plots.{LinePlot, LightPlotTheme, Legend}
		
		val labels = Legend(
			name,
			"Scala parallel collections",
			s"Scala parallel computation for $label",
			"Relative timing"
		)
		LinePlot.display(x.toVector, labels, new LightPlotTheme)
	}
}


// -------------------------------------------  EOF --------------------------------------------------
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

import org.scalaml.scalability.scala.{ParArrayBenchmark, ParMapBenchmark}
import org.scalaml.app.Eval
import org.scalaml.util.DisplayUtils
import org.scalaml.core.Types.ScalaMl.DblVector

	/**
	 * <p><b>Purpose</b>: Singleton to evaluate the performance of Scala parallel arrays
	 * and maps.</p>
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

	 	/** <p>Execution of the scalatest for Master-worker design with Akka framework.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */	
	def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Scala parallel collections", logger)
		 
		if( args.size > 0) {
			// Arbitrary map function
			val mapF = (x: Double) => Math.sin(x*0.01) + Math.exp(-x)
				// Arbitrary filter function
			val filterF = (x: Double) => (x > 0.8)
			 	// Arbitrary reduce function
			val reduceF = (x:Double, y:Double) => (x+y)*x
			 
			DisplayUtils.show(s"$name Comparative benchmark for $NUM_TASKS tasks\nIter\tRatio", logger)
			Try {
				if(args(0) == "array")
					evaluateParArray(mapF, filterF, reduceF)
				else
					evaluateParMap(mapF, filterF)
				0
			} match {
			  case Success(n) => n
			  case Failure(e) => failureHandler(e)
			}
				
		 }
		 else 
			 DisplayUtils.error(s"$name Incorrect cmd line argument. It should be 'array' or 'map'", logger)
	}
	 
	private def evaluateParArray(mapF: Double => Double, filterF: Double => Boolean, reduceF: (Double, Double) => Double): Unit =  {
			// Generate random vector for both the non-parallel and parallel array
		 val data = Array.fill(SZ)(Random.nextDouble)
		 val pData = ParArray.fill(SZ)(Random.nextDouble)
	
		 	// Initialized and execute the benchmark for the parallel array
		 val benchmark = new ParArrayBenchmark[Double](data, pData, TIMES)
		 
		 DisplayUtils.show("Mapper for (x: Double) => Math.sin(x*0.01) + Math.exp(-x)", logger)
		 val ratios = new Array[Double](NUM_TASKS)
		 evalRange.foreach(n => ratios.update(n, benchmark.map(mapF)(n)))
		 display(ratios.drop(1), "ParArray.map")
		 
		 DisplayUtils.show("Filter for (x: Double) => (x > 0.8)", logger)
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
		 DisplayUtils.show("Mapper for (x: Double) => Math.sin(x*0.01) + Math.exp(-x)", logger)
	
		 val ratios = new Array[Double](NUM_TASKS)
		 evalRange.foreach(n => ratios.update(n, benchmark.map(mapF)(n)))
		 display(ratios.drop(1), "ParMap.map")
		 
		 DisplayUtils.show("Filter for (x: Double) => (x > 0.8)", logger)
		 evalRange.foreach(n => ratios.update(n, benchmark.filter(filterF)(n)))
		 display(ratios.drop(1), "ParMap.filter")
	}
	
	
	private def display(x: DblVector, label: String): Unit =   {
		import org.scalaml.plots.{LinePlot, LightPlotTheme}
		val labels = List[String](
			name,
			"Scala parallel collections",
			s"Number of tasks for $label",
			"Relative timing"
		)
		LinePlot.display(x, labels, new LightPlotTheme)
	}
}

// -------------------------------------------  EOF --------------------------------------------------
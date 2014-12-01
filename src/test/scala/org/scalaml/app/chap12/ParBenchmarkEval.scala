/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96c
 */
package org.scalaml.app.chap12

import scala.collection.mutable.HashMap
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.mutable.ParArray
import scala.collection.parallel.ForkJoinTaskSupport
import org.scalaml.scalability.scala.ParArrayBenchmark
import scala.collection.parallel.mutable.ParHashMap
import org.scalaml.scalability.scala.ParMapBenchmark
import scala.concurrent.forkjoin.ForkJoinPool
import org.scalaml.app.Eval
import org.apache.log4j.Logger
import org.scalaml.util.Display






object ParBenchmarkEval extends Eval {
	 import scala.util.Random
	 val name: String = "ParBenchmarkeval"
	 val maxExecutionTime: Int = 10000
	   	
	 private val logger = Logger.getLogger(name)
	 private val SZ = 1000000
	 private val NUM_TASKS = 16
	 private val evalRange = Range(1, NUM_TASKS)

	 	/** <p>Execution of the scalatest for Master-worker design with Akka framework.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	 def run(args: Array[String]): Int = {
		 Display.show(s"\n\n *****  test#${Eval.testCount} $name Scala parallel collections", logger)
		 
		 val mapper = new HashMap[String, Double]
		 val mapped = mapper.map( k => (k._1, k._2/10.0))
		 
		 val rand = new ParVector[Float]
		 Range(0, SZ).foreach(n => rand.updated(n, n*Random.nextFloat))
		 rand.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(NUM_TASKS))
		 val randExp = rand.map( Math.exp(_) )
	
		 val data = Array.fill(SZ)(Random.nextDouble)
		 val pData = ParArray.fill(SZ)(Random.nextDouble)
		 val times: Int = 25
		 	 
		 val benchmark = new ParArrayBenchmark[Double](data, pData, times)
		 
		 val mapF = (x: Double) => Math.sin(x*0.01) + Math.exp(-x)
		 evalRange.foreach(n => benchmark.map(mapF)(n))
	
		 val filterF = (x: Double) => (x > 0.8)
		 evalRange.foreach(n => benchmark.filter(filterF)(n))
		 
		 val mapData = new HashMap[Int, Double]
		 Range(0, SZ).foreach(n => mapData.put(n, Random.nextDouble) )
		 val parMapData = new ParHashMap[Int, Double]
		 Range(0, SZ).foreach(n => parMapData.put(n, Random.nextDouble) )
		 
		 val benchmark2 = new ParMapBenchmark[Double](mapData, parMapData, times)
		 evalRange.foreach(n => benchmark2.map(mapF)(n))
		 evalRange.foreach(n => benchmark2.filter(filterF)(n))
		 1
	 }
}


// -------------------------------------------  EOF --------------------------------------------------
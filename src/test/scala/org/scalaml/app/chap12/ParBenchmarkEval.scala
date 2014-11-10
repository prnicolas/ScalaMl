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
	 private val logger = Logger.getLogger(name)
	 
	 
     def run(args: Array[String]): Int = {
		 Display.show(s"$name evaluation Scala parallel collections", logger)
		 
		 val mapper = new HashMap[String, Double]
		 val mapped = mapper.map( k => (k._1, k._2/10.0))
		 
		 val rand = new ParVector[Float]
		 Range(0, 1000000).foreach(n => rand.updated(n, n*Random.nextFloat))
		 rand.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(16))
		 val randExp = rand.map( Math.exp(_) )
	
		  
		 val sz = 1000000
		 val data = Array.fill(sz)(Random.nextDouble)
		 val pData = ParArray.fill(sz)(Random.nextDouble)
		 val times: Int = 25
		 	 
		 val benchmark = new ParArrayBenchmark[Double](data, pData, times)
		 
		 val mapF = (x: Double) => Math.sin(x*0.01) + Math.exp(-x)
		 Range(1, 16).foreach(n => benchmark.map(mapF)(n))
	
		 val filterF = (x: Double) => (x > 0.8)
		 Range(1, 16).foreach(n => benchmark.filter(filterF)(n))
		 
		 val mapData = new HashMap[Int, Double]
		 Range(0, sz).foreach(n => mapData.put(n, Random.nextDouble) )
		 val parMapData = new ParHashMap[Int, Double]
		 Range(0, sz).foreach(n => parMapData.put(n, Random.nextDouble) )
		 
		 val benchmark2 = new ParMapBenchmark[Double](mapData, parMapData, times)
		 Range(1, 16).foreach(n => benchmark2.map(mapF)(n))
		 Range(1, 16).foreach(n => benchmark2.filter(filterF)(n))
		 1
	 }
}





// -------------------------------------------  EOF --------------------------------------------------
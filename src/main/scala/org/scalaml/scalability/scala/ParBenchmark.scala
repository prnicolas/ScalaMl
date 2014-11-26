/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96a
 */
package org.scalaml.scalability.scala


import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import scala.Array.fallbackCanBuildFrom
import scala.collection.parallel.mutable.{ParArray, ParMap, ParHashMap}
import scala.collection.mutable.{Map, HashMap}
import org.scalaml.util.Display
import scala.collection.parallel.immutable.ParVector
import org.apache.log4j.Logger



		/**
		 * <p>Generic benchmark for evaluating the performance of Scala parallel collections.</p>
		 * @constructor Create a performance benchmark. [times] Number of executions to be performed during the performance testing
		 * @throws IllegalArgumentException if the number of executions is out of range
		 * @param times Number of executions to be performed during the performance testing
		 * 
		 * @author Patrick Nicolas
		 * @since March 17, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks/Scala/Parallel collections
		 */
abstract class ParBenchmark[U](times: Int) {
	require(times > 0 && times < 512, s"ParBenchmark number of executions $times is out of range")
	
		/**
		 * <p>Define the map operator for the performance benchmark</p>
		 * @param f function invoked by map
		 * @param nTasks number of concurrent tasks to implement the operator
		 */
	def map(f: U => U)(nTasks: Int): Unit 
	
		/**
		 * <p>Define the filter operator for the performance benchmark</p>
		 * @param f function invoked by filter method
		 * @param nTasks number of concurrent tasks to implement the operator
		 */
	def filter(f: U => Boolean)(nTasks: Int): Unit
   
	
		/**<p>Method to compute the execution time for a higher order Scala method 
		 * invoking a predefined function g</p>
		 * @param function invoked by map or filter during performance test
		 * @return Duration of the execution in milliseconds
		 */
	protected def timing(g: Int => Unit ): Long = {
		require(g != null, "ParBenchmark.timing argument undefined")
		
		var startTime = System.currentTimeMillis
		Range(0, times).foreach(g)
		System.currentTimeMillis - startTime
	}
}

		/**
		 * <p>Class to evaluate the performance of the Scala parallel arrays. The
		 * class override the map, reduceLeft methods to collect timing information.</p>
		 * @constructor Create a performance benchmark for Scala arrays. 
		 * @param u  Parameterized array
		 * @param v Parameterized parallel array
		 * @param times Number of executions in the performance test.
		 * @throws IllegalArgumentException if the array of elements is undefined or the number of tasks is out of range
		 * @author Patrick Nicolas
		 * @since March 17, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks/Scala/Parallel collections
		 */
class ParArrayBenchmark[U](u: Array[U], v: ParArray[U], times: Int) extends ParBenchmark[U](times) {
	import ParArrayBenchmark._
	
	check(u,v)
	private val logger = Logger.getLogger("ParArrayBenchmark")
	
		/**
		 * <p>Define the map operator for the performance benchmark of the Scala array</p>
		 * @param f function invoked by map
		 * @param nTasks number of concurrent tasks to implement the operator
		 */
	override def map(f: U => U)(nTasks: Int): Unit = {
		require(f != null, "ParArrayBenchmark.map: Cannot execute a map on the data set with undefined operator" )
		require(nTasks > 0 && nTasks < MAX_NUM_TASKS, s"ParArrayBenchmark.map number of concurrent tasks $nTasks is out of range")
		v.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nTasks))

		val duration = timing(_ => u.map(f)).toDouble
		val ratio = timing( _ => v.map(f) )/duration
		Display.show(s"$nTasks, $ratio", logger)
	}
	
		/**
		 * <p>Define the filter operator for the performance benchmark of Scala arrays</p>
		 * @param f function invoked by filter method
		 * @param nTasks number of concurrent tasks to implement the operator
		 */
	override def filter(f: U => Boolean)(nTasks: Int): Unit = {
		require(f != null, "ParArrayBenchmark.filter: Cannot reduce data set with undefined operator" )
		require(nTasks > 0 && nTasks < MAX_NUM_TASKS, s"ParArrayBenchmark.filter number of concurrent tasks $nTasks is out of range")
		
		v.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nTasks))

		val duration = timing(_ => u.filter(f)).toDouble
		val ratio = timing( _ => v.filter(f) )/duration
		Display.show(s"$nTasks, $ratio", logger)
	}
	
		/**
		 * <p>Implements a reducer operator for the performance benchmark of Scala arrays</p>
		 * @param f function invoked by the reducer
		 * @param nTasks number of concurrent tasks to implement the operator
		 */
	def reduce(f: (U,U) => U)(nTasks: Int): Unit = {
		require(f != null, "ParArrayBenchmark: Cannot reduce data set with undefined operator" )	
		v.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nTasks))

		val duration = timing(_ => u.reduceLeft(f)).toDouble
		val ratio = timing( _ => v.reduceLeft(f) )/duration
		Display.show(s"$nTasks, $ratio", logger)
	}
}


object ParArrayBenchmark {
	val MAX_NUM_TASKS = 64
	
	protected def check[U](u: Array[U], v: ParArray[U]): Unit = {
		require(u != null && u.size > 0, "ParArrayBenchmark: Array in performance testing of Scala parallel collections undefined")
		require(v != null && v.size > 0, "ParArrayBenchmark: Parallel Array in performance testing of Scala parallel collections undefined")
		require(u.size == v.size, "ParArrayBenchmark: Size of the array ${u.size} is different from the size of the parallel array ${v.size}")
	}
}


		/**
		 * <p>Class to evaluate the performance of the Scala parallel map. The
		 * class override the map, reduceLeft methods to collect timing information</p>
		 * @constructor Create a performance benchmark for Scala arrays. [u] Parameterized map of type Map[Int,U], [v] Parameterized parallel map. [_times] Number of executions in the performance test.
		 * @throws IllegalArgumentException if the array of elements is undefined or the number of tasks is out of range
		 * @param u  Parameterized map, <b>Map[Int, U]<\b>
		 * @param v Parameterized parallel map
		 * @param times Number of executions in the performance test.
		 * @author Patrick Nicolas
		 * @since March 17, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks/Scala/Parallel collections
		 */
final class ParMapBenchmark[U](u: Map[Int, U], v: ParMap[Int, U], times: Int) extends ParBenchmark[U](times) {
	import ParMapBenchmark._
	
	check(u,v)
    private val logger = Logger.getLogger("ParMapBenchmark")
    	
    	/**
		 * <p>Define the map operator for the performance benchmark of the Scala map</p>
		 * @param f function invoked by map
		 * @param nTasks number of concurrent tasks to implement the operator
		 */
	override def map(f: U=> U)(nTasks: Int): Unit = {
		require(f != null, "ParMapBenchmark: Cannot execute a map on the data set with undefined operator" )
		require(nTasks > 0 && nTasks < MAX_NUM_TASKS, s"ParMapBenchmark.map number of concurrent tasks $nTasks is out of range")
		
		v.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nTasks))
		val duration = timing(_ => u.map(e => (e._1, f(e._2)))).toDouble
		val ratio = timing( _ => v.map(e => (e._1, f(e._2))) )/duration
		Display.show(s"$nTasks, $ratio", logger)
	}
	
		/**
		 * <p>Define the filter operator for the performance benchmark of Scala map</p>
		 * @param f function invoked by filter method
		 * @param nTasks number of concurrent tasks to implement the operator
		 */
	override def filter( f: U => Boolean)(nTasks: Int): Unit = {
		require(f != null, "ParMapBenchmark: Cannot execute a map on the data set with undefined operator" )
		require(nTasks > 0 && nTasks < MAX_NUM_TASKS, s"ParMapBenchmark.filter number of concurrent tasks $nTasks is out of range")
		
		v.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nTasks))
		val duration = timing(_ => u.filter(e => f(e._2))).toDouble
		val ratio = timing( _ => v.filter(e => f(e._2)))/duration
		Display.show(s"$nTasks, $ratio", logger)
	}
}


object ParMapBenchmark {
	val MAX_NUM_TASKS = 64
	
	protected def check[U](u: Map[Int, U], v: ParMap[Int, U]): Unit = {
		require(u != null && u.size > 0, "ParMapBenchmark: Cannot evaluate performance of Scala parallel map with undefined data")
		require(v != null && v.size > 0, "ParMapBenchmark: Parallel map in performance testing ")
		require(u.size == v.size, "ParMapBenchmark: Size of the map ${u.size} is different from the size of the parallel map ${v.size}")
	}
}

// ---------------------------------  EOF -------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95
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



abstract class ParBenchmark[U](times: Int) {
   def map(f: U => U)(nTasks: Int): Unit 
   def filter(f: U => Boolean)(nTasks: Int): Unit
   
   protected def timing(g: Int => Unit ): Long = {
       var startTime = System.currentTimeMillis
	   Range(0, times).foreach(g)
	   System.currentTimeMillis - startTime
    }
}

		/**
		 * <p>Class to evaluate the performance of Scala parallel collections. The
		 * class override the map, reduceLeft methods to collect timing information</p>
		 * @param t  Array of elements of type inherited from Double.
		 * @param numTasks  Number of concurrent task allocated to the high order method.
		 * @throws IllegalArgumentException if the array of elements is undefined or the number of tasks is out of range
		 * 
		 * @author Patrick Nicolas
		 * @since March 17, 2014
		 * @note Scala for Machine Learning
		 */
class ParArrayBenchmark[U](val u: Array[U], val v: ParArray[U], _times: Int) extends ParBenchmark[U](_times) {
	require(u != null && u.size > 0, "ParArrayBenchmark: Cannot evaluate performance of Scala parallel collections with undefined data")
	private val logger = Logger.getLogger("ParArrayBenchmark")
	
	override def map(f: U => U)(nTasks: Int): Unit = {
	   require(f != null, "ParArrayBenchmark:  Cannot execute a map on the data set with undefined operator" )
	   v.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nTasks))
	   
	   val duration = timing(_ => u.map(f)).toDouble
	   val ratio = timing( _ => v.map(f) )/duration
       Display.show(s"$nTasks, $ratio", logger)
	}
	
	override def filter(f: U => Boolean)(nTasks: Int): Unit = {
      require(f != null, "ParArrayBenchmark: Cannot reduce data set with undefined operator" )	
      v.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nTasks))
      
       val duration = timing(_ => u.filter(f)).toDouble
       val ratio = timing( _ => v.filter(f) )/duration
       Display.show(s"$nTasks, $ratio", logger)
    }
	
    def reduce(f: (U,U) => U)(nTasks: Int): Unit = {
      require(f != null, "ParArrayBenchmark: Cannot reduce data set with undefined operator" )	
      v.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nTasks))
      
       val duration = timing(_ => u.reduceLeft(f)).toDouble
       val ratio = timing( _ => v.reduceLeft(f) )/duration
       Display.show(s"$nTasks, $ratio", logger)
    }

}

final class ParMapBenchmark[U](val u: Map[Int, U], val v: ParMap[Int, U], _times: Int) extends ParBenchmark[U](_times) {
	require(u != null && u.size > 0, "ParArrayBenchmark: Cannot evaluate performance of Scala parallel collections with undefined data")
    private val logger = Logger.getLogger("ParMapBenchmark")
    	
	override def map(f: U=> U)(nTasks: Int): Unit = {
	   require(f != null, "ParArrayBenchmark: Cannot execute a map on the data set with undefined operator" )
	   v.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nTasks))
	   
	   val duration = timing(_ => u.map(e => (e._1, f(e._2)))).toDouble
	   val ratio = timing( _ => v.map(e => (e._1, f(e._2))) )/duration
       Display.show(s"$nTasks, $ratio", logger)
	}
	
	override def filter( f: U => Boolean)(nTasks: Int): Unit = {
      require(f != null, "ParArrayBenchmark: Cannot execute a map on the data set with undefined operator" )
	   v.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nTasks))
	   
	   val duration = timing(_ => u.filter(e => f(e._2))).toDouble
	   val ratio = timing( _ => v.filter(e => f(e._2)))/duration
       Display.show(s"$nTasks, $ratio", logger)
	}
}


// ---------------------------------  EOF -------------------------
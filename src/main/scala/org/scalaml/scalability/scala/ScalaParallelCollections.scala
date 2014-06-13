/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.scalability.scala


import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool
import scala.Array.fallbackCanBuildFrom


		/**
		 * <p>Class to evaluate the performance of Scala parallel collections. The
		 * class override the map, reduceLeft methods to collect timing information</p>
		 * @param t  Array of elements of type inherited from Double.
		 * @param numTasks  Number of concurrent task allocated to the high order method.
		 * @exception IllegalArgumentException if the array of elements is undefined or the number of tasks is out of range
		 * 
		 * @author Patrick Nicolas
		 * @date March 17, 2014
		 * @project Scala for Machine Learning
		 */
class ParallelCollections[U <: Double](val u: Array[U], numTasks: Int = 1) {
	require(u != null && u.size > 0, "Cannot evaluate prrformance of Scala parallel collections with undefined data")
	require(numTasks > 0 && numTasks < 32, "The number of tasks used to evaluate the Scala parallel collections " + numTasks + " is out of range")
	
	
	def map(f: U => U): Unit = {
	   require(f != null, "Cannot execute a map on the data set with undefined operator" )
	   
	   val startTime = System.currentTimeMillis
	   u map f
	   val midTime = System.currentTimeMillis
	   val tpar = u.par
	   tpar.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(numTasks))
		 def run = tpar map f 

	   println(u.size + "," + (midTime - startTime).toString + "," + (System.currentTimeMillis - midTime).toString)
	}
	
    def reduceLeft(f: (U,U) => U): Unit = {
      require(f != null, "Cannot reduce data set with undefined operator" )	
    	
       val startTime = System.currentTimeMillis
	   u reduceLeft f
	   val midTime = System.currentTimeMillis
	   val tpar = u.par
	   tpar.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(numTasks))
		 def run = tpar reduceLeft f 

	   println(u.size + "," + (midTime - startTime).toString + "," + (System.currentTimeMillis - midTime).toString)
    }
}


// ---------------------------------  EOF -------------------------
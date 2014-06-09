/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.util

import scala.collection.mutable.HashMap

/**
 *  @author Patrick Nicolas
 *  @date Feb 3, 2014
 *  @project Book
 */
class Counter[T] extends HashMap[T, Int] { 
   def += (t: T): Unit = put(t, getOrElse(t, 0)+1) 
   def + (t: T): Counter[T] = { put(t, getOrElse(t, 0)+1); this }
   def ++ (cnt: Counter[T]): Counter[T] = { cnt.foldLeft(this)((c, t) => c + t._1); this}
   
   def / (cnt: Counter[T]): HashMap[T, Double] = 
  	 map( x => (x._1, if( !cnt.contains(x._1) ) throw new IllegalStateException("Incomplete counter") else x._2.toDouble/cnt.get(x._1).get ) )
   
   override def apply(t: T): Int = getOrElse(t, 0)  
}



object CounterApp extends App {
  val counter = new Counter[Int] 
  counter += 1
  counter += 2
  counter += 4
  
  counter foreach( kv => println( kv._1 + "," + kv._2))
}

// --------------------------  EOF ------------------------------------------------
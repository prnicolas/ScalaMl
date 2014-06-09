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
 *  @date Mar 1, 2014
 *  @project Book
 */
class Accumulator[T] extends HashMap[T, List[T]] {
  
   override def put(n: T, ls: List[T]): Option[List[T]] = {
  	 val xs = getOrElse(n, List[T]())
  	 super.put(n, ls ::: xs)
   }
   
   def add(n: T, m: T): Option[List[T]] = {
  	 val xs = getOrElse(n, List[T]())
  	 super.put(n, m :: xs)
   }
}

// ---------------------  EOF --------------------------------
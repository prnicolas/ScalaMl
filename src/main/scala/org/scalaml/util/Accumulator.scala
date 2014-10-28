/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.94
 */
package org.scalaml.util

import scala.collection.mutable.HashMap

/**
 *  @author Patrick Nicolas
 *  @since Mar 1, 2014
 *  @note Book
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


class NumericAccumulator extends HashMap[Int, (Int, Double)] {
	  
  def +=(t: Int, x: Double): Option[(Int, Double)] = {
  	 val newValue = if(contains(t)) (get(t).get._1+1, get(t).get._2 + x) else (1, x)
  	 super.put(t, newValue)
  }
}




// ---------------------  EOF --------------------------------
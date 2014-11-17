/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 */
package org.scalaml.util

import scala.collection.mutable.HashMap

		/**
		 * <p>Count implemented as a Hash map of type <T, Int>. Contrary to a Hash map, an accumulator
		 * is additive: Elements can share the same key.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since March 1, 2014
		 * @note Scala for Machine Learning 
		 */
final class Counter[T] extends HashMap[T, Int] { 
	def += (t: T): Unit = super.put(t, getOrElse(t, 0)+1) 
   
	def + (t: T): Counter[T] = { super.put(t, getOrElse(t, 0)+1); this }

	def ++ (cnt: Counter[T]): Counter[T] = { cnt.foldLeft(this)((c, t) => c + t._1); this}
   
	def / (cnt: Counter[T]): HashMap[T, Double] = 
		map( x => (x._1, if( !cnt.contains(x._1) ) throw new IllegalStateException("Counter./ Incomplete counter") else x._2.toDouble/cnt.get(x._1).get ) )
   
	override def apply(t: T): Int = getOrElse(t, 0)  
}



// --------------------------  EOF ------------------------------------------------
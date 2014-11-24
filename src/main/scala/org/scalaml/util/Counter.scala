/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
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
  
		/**
		 * Add a new element to this Counter
		 * @param t New element to be added
		 * @return New element if the insertion succeeds, Null otherwise
		 */
	def += (t: T): Unit = super.put(t, getOrElse(t, 0)+1) 
   
		 /**
		 * Concatenate this counter with a new element and returns a new counter
		 * @param t New element to be added
		 * @return New counter with the inserted element
		 */
	def + (t: T): Counter[T] = { super.put(t, getOrElse(t, 0)+1); this }

	
		 /**
		 * Concatenate this counter and another counter and returns the result
		 * into a new counter
		 * @param cnt Counter to aggregate to this counter
		 * @return New counter as the aggreate of this counter and cnt
		 */
	def ++ (cnt: Counter[T]): Counter[T] = { cnt.foldLeft(this)((c, t) => c + t._1); this}

		/** divide the elements of this counter by the corresponding
		 *  elements in another counter
		 *  @param cnt Counter which elements are used to divided corresponding elements of this counter
		 *  @return HashMap of key of type T and value as the quotient of the elements of this counter by the corresponding elemetns in cnt
		 */
	def / (cnt: Counter[T]): HashMap[T, Double] = 
		map( x => (x._1, if( !cnt.contains(x._1) ) 
			throw new IllegalStateException("Counter./ Incomplete counter")
		else 
			x._2.toDouble/cnt.get(x._1).get ) )
   
	override def apply(t: T): Int = getOrElse(t, 0)  
}



// --------------------------  EOF ------------------------------------------------
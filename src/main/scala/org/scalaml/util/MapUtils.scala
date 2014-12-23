/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.util

import scala.collection._


		/**
		 * Singleton the define specialized mutable HashMap (counter and accumulator). Those
		 * class are similar to the Counter and Accumulator used in Apache Spark
		 * 
		 * @author Patrick Nicolas
		 * @since July 11, 2014
		 * @note Scala for Machine Learning
		 */
object MapUtils {

		/**
		 * <p>Count implemented as a Hash map of type <T, Int>. Contrary to a Hash map, an accumulator
		 * is additive: Elements can share the same key.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since March 1, 2014
		 * @note Scala for Machine Learning 
		 */
	final class Counter[T] extends mutable.HashMap[T, Int] {
	  
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
			 *  @return HashMap of key of type T and value as the quotient of the elements of this 
			 *  counter by the corresponding elemetns in cnt
			 */
		def / (cnt: Counter[T]): mutable.HashMap[T, Double] = 
			//map( x => (x._1, {assert( cnt.contains(x._1) ); x._2.toDouble/cnt.get(x._1).get} ) ) 
			
			map( x => (x._1, if( !cnt.contains(x._1) ) 
				throw new IllegalStateException("Counter./ Incomplete counter")
			else 
				x._2.toDouble/cnt.get(x._1).get ) )
	
	   
		override def apply(t: T): Int = getOrElse(t, 0)  
	}

	
		/**
		 * <p>Accumulator implemented as a Hash map of type <T, List[T]>. Contrary to a Hash map, 
		 * an accumulator is additive: Elements can share the same key.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since March 1, 2014
		 * @note Scala for Machine Learning 
		 */
	final class Accumulator[T] extends mutable.HashMap[T, List[T]] {
	
			/**
			 * Override the put method of the Hash map to enforce additive update
			 * @param key key of the new element
			 * @param _xs value of the new element of type List
			 * @return Option of the updated list if method is successful, None otherwies
			 */
		override def put(key: T, _xs: List[T]): Option[List[T]] = {
			val xs = getOrElse(key, List[T]())
			super.put(key, _xs ::: xs)
		}
	   
			/**
			 * Override the put method of the Hash map to enforce additive update of the accumulator
			 * with a element which value is identical to the key.
			 * @param key key of the new element
			 * @param t value of the new element with the same type as the key
			 * @return Option of the updated list if method is successful, None otherwies
			 */
		def add(key: T, t: T): Option[List[T]] = {
			val xs = getOrElse(key, List[T]())
			super.put(key, t :: xs)
		}
	}
	
	
			/**
			 * <p>Accumulator implemented as a Hash map to update a value as a tuple
			 * <counter, Double value>. The accumulator is additive: Elements can share the same key.</p>
			 * 
			 * @author Patrick Nicolas
			 * @since March 1, 2014
			 * @note Scala for Machine Learning 
			 */
	final class NumericAccumulator[T] extends mutable.HashMap[T, (Int, Double)] {
		
			/**
			 * Update this Numeric accumulator with a key of parameterized type and
			 * a double value. This operation is additive as both the counter and 
			 * the value are updated.
			 * @param key key of the new element
			 * @param x value of the new element, to be added to the existing values that share the 
			 * same key.
			 */
		def +=(key: T, x: Double): Option[(Int, Double)] = {
			val newValue = if(contains(key)) (get(key).get._1+1, get(key).get._2 + x) else (1, x)
			super.put(key, newValue)
		}
	}
}

// -------------------------------- EOF -----------------------------
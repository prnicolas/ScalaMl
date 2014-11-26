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
package org.scalaml.util

import scala.collection.mutable.HashMap

		/**
		 * <p>Accumulator implemented as a Hash map of type <T, List[T]>. Contrary to a Hash map, an accumulator
		 * is additive: Elements can share the same key.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since March 1, 2014
		 * @note Scala for Machine Learning 
		 */
final class Accumulator[T] extends HashMap[T, List[T]] {

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
final class NumericAccumulator[T] extends HashMap[T, (Int, Double)] {
	
		/**
		 * Update this Numeric accumulator with a key of parameterized type and
		 * a double value. This operation is additive as both the counter and 
		 * the value are updated.
		 * @param key key of the new element
		 * @param x value of the new element, to be added to the existing values that share the same key.
		 */
	def +=(key: T, x: Double): Option[(Int, Double)] = {
		val newValue = if(contains(key)) (get(key).get._1+1, get(key).get._2 + x) else (1, x)
		super.put(key, newValue)
	}
}


// ---------------------  EOF --------------------------------
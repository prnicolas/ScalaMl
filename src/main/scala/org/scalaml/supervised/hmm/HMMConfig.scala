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
package org.scalaml.supervised.hmm


import org.scalaml.supervised.Supervised
import org.scalaml.core.Design.Config


		/**
		 * <p>Utility class that defined the dimension of the matrix
		 * used in the Hidden Markov Model. The terminology used in the code follows
		 * the naming convention used in the mathematical expressions presented in
		 * most of papers and technical books on HMM.</p>
		 * @constructor Create a configuration (dimensions) for this HMM
		 * @param _T  Number of observations
		 * @param _N   Number of hidden states in the HMM
		 * @param _M   Number of symbols (or model dimension) for the HMM
		 * @throws IllegalArgumenException if any of the argument is out of range [1, 1000]
		 * @see org.scalaml.core.Design.Config
		 * 
		 * @author Patrick Nicolas
		 * @since March 27, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model
		 */
final class HMMConfig(val _T: Int, val _N: Int, val _M: Int) extends Config {
	import HMMConfig._
	
	check(_T, _N, _M)
}


		/**
		 * <p>Companion object for HMMConfig to implement high order method for
		 * HMMConfig such as foreach, fold and maxBy.
		 * @author Patrick Nicolas
		 * @since March 27, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model
		 */
object HMMConfig {

		/**
		 * Defines the <b>foreach</b> iterator for the elements of a collection between two index
		 * @param i starting index for the iterator
		 * @param j ending index for the iterator
		 * @param f function executed of each element
		 */
	def foreach(i: Int, j: Int, f: (Int) => Unit): Unit = Range(i, j).foreach(f)
		/**
		 * Defines the <b>foreach</b> iterator for the first j elements of a collection 
		 * @param j ending index for the iterator
		 * @param f function executed of each element
		 */
	def foreach(j: Int, f: (Int) => Unit): Unit = foreach(0, j, f)
	
		/**
		 * Implements a fold operator on the first j elements of a collection 
		 * @param j ending index for the iterator
		 * @param f reducer function/aggregator executed of each element
		 * @param zero Initial value for the fold
		 */
	def foldLeft(j: Int, f: (Double, Int) => Double, zero: Double) = Range(0, j).foldLeft(zero)(f)
	
		/**
		 * Implements a fold operator on the first j elements of a collection, initialized to 0
		 * @param j ending index for the iterator
		 * @param f reducer function/aggregation executed of each element
		 */
	def foldLeft(j: Int, f: (Double, Int) => Double) = Range(0, j).foldLeft(0.0)(f)
	
		/**
		 * Compute the maximum value of the first j elements of a collection
		 * @param j ending index for the iterator
		 * @param f scoring function executed of each element
		 */
	def maxBy(j: Int, f: Int => Double): Int = Range(0, j).maxBy(f)
   
	val MAX_NUM_STATES = 512
	val MAX_NUM_OBS = 4096
	private def check(_T: Int, _N: Int, _M: Int): Unit =  {
		require( _T > 0 && _T < MAX_NUM_OBS, 
		    s"Number of observations ${_T} in HMM lambda model is out of bounds")
		require( _N > 0 && _N < MAX_NUM_STATES, 
		    s"Number of States ${_N} in HMM lambda model is out of bounds")
		require( _M > 0 && _M < MAX_NUM_OBS, 
		    s"Number of symbols ${_M} in HMM lambda model is out of bounds")
	}
}

// ----------------------------------------  EOF ------------------------------------------------------------
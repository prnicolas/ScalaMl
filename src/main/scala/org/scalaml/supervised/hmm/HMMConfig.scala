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
package org.scalaml.supervised.hmm


import org.scalaml.supervised.Supervised
import org.scalaml.core.design.Config



		/**
		 * <p>Utility class that defined the dimemsion of the matrix
		 * used in the Hidden Markov Model. The terminology used in the code follows
		 * the naming convention used in the mathematical expressions presented in
		 * most of papers and technical books on HMM as well as the book<br>
		 * <b>_T</b> Number of observations<br>
		 * <b>_N</b> Number of hidden states in the HMM<br>
		 * <b>_M</b> Number of symbols (or model dimension) for the HMM</p>
		 * @constructor Create a configuration (dimensions) for this HMM
		 * @throws IllegalArgumenException if any of the argument is out of range [1, 1000]
		 * @author Patrick Nicolas
		 * @since March 27, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models/Hidden Markov Model
		 */
class HMMConfig(val _T: Int, val _N: Int, val _M: Int) extends Config {
	import HMMConfig._
	
	check(_T, _N, _M)
	val persists = "config/hmm"
}


		/**
		 * <p>Companion object for HMMConfig to implement high order method for
		 * HMMConfig such as foreach, fold and maxBy.
		 */
object HMMConfig {
	def foreach(i: Int, j: Int, f: (Int) => Unit): Unit = Range(i, j).foreach(f)
	def foreach(i: Int, f: (Int) => Unit): Unit = foreach(0, i, f)  
	def foldLeft(i: Int, f: (Double, Int) => Double, zero:Double) = Range(0, i).foldLeft(zero)(f)
	def foldLeft(i: Int, f: (Double, Int) => Double) = Range(0, i).foldLeft(0.0)(f)
	def maxBy(i: Int, f: Int => Double): Int = Range(0, i).maxBy(f)
   
	val MAX_NUM_STATES = 512
	val MAX_NUM_OBS = 4096
	private def check(_T: Int, _N: Int, _M: Int): Unit =  {
		require( _T > 0 && _T < MAX_NUM_OBS, s"Number of observations ${_T} in HMM lambda model is out of bounds")
		require( _N > 0 && _N < MAX_NUM_STATES, s"Number of States ${_N} in HMM lambda model is out of bounds")
		require( _M > 0 && _M < MAX_NUM_OBS, s"Number of symbols ${_M} in HMM lambda model is out of bounds")
	}
}

// ----------------------------------------  EOF ------------------------------------------------------------
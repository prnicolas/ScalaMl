/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99.1
 */
package org.scalaml.supervised.hmm

import org.scalaml.core.Design.Config


		/**
		 * Utility class that defined the dimension of the matrix
		 * used in the Hidden Markov Model. The terminology used in the code follows
		 * the naming convention used in the mathematical expressions presented in
		 * most of papers and technical books on HMM.
		 * @constructor Create a configuration (dimensions) for this HMM
		 * @param numObs Number of observations
		 * @param numStates  Number of hidden states in the HMM
		 * @param numSymbols  Number of symbols (or model dimension) for the HMM
		 * @param maxIters Maximum number of iterations or recursion for the Baum-Welch training
		 * @param eps Convergence criteria for the training of the HMM
		 * @throws IllegalArgumentException if any of the argument is out of range [1, 1000]
		 * @see org.scalaml.core.Design.Config
		 * 
		 * @author Patrick Nicolas
		 * @since March 27, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model
		 */
case class HMMConfig(
		numObs: Int,
		numStates: Int,
		numSymbols: Int,
		maxIters: Int,
		eps: Double) extends Config {
	import HMMConfig._
	
	check(numObs, numStates, numObs, maxIters, eps)
}


		/**
		 * Companion object for HMMConfig to implement high order method for
		 * HMMConfig such as foreach, fold and maxBy.
		 * @author Patrick Nicolas
		 * @since March 27, 2014
		 * @see Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov Model
		 */
object HMMConfig {

		/**
		 * Defines the '''foreach''' iterator for the elements of a collection between two index
		 * @param i starting index for the iterator
		 * @param j ending index for the iterator
		 * @param f function executed of each element
		 */
	def foreach(i: Int, j: Int, f: (Int) => Unit): Unit = Range(i, j).foreach(f)
		/**
		 * Defines the '''foreach''' iterator for the first j elements of a collection 
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
	def /:(j: Int, f: (Double, Int) => Double, zero: Double) = Range(0, j)./:(zero)(f)
	
		/**
		 * Implements a fold operator on the first j elements of a collection, initialized to 0
		 * @param j ending index for the iterator
		 * @param f reducer function/aggregation executed of each element
		 */
	def /:(j: Int, f: (Double, Int) => Double) = Range(0, j)./:(0.0)(f)
	
		/**
		 * Compute the maximum value of the first j elements of a collection
		 * @param j ending index for the iterator
		 * @param f scoring function executed of each element
		 */
	def maxBy(j: Int, f: Int => Double): Int = Range(0, j).maxBy(f)
   
	val MAX_NUM_STATES = 512
	val MAX_NUM_OBS = 4096
	val MAX_NUM_SYMBOLS = 128
	val MAX_NUM_ITERS = 1024
	val MAX_EPS = 1e-1
	
	private def check(numObs: Int, numStates: Int, numSymbols: Int, maxIters: Int, eps: Double): Unit =  {
		require(numObs > 0 && numObs < MAX_NUM_OBS, 
			s"HMMConfig: numObs found $numObs , required 0 <  < $MAX_NUM_OBS")
		require(numStates > 0 && numStates < MAX_NUM_STATES, 
			s"HMMConfig: numStates found $numStates , required 0 <  < $MAX_NUM_STATES")
		require(numSymbols > 0 && numSymbols < MAX_NUM_SYMBOLS, 
			s"HMMConfig: numSymbols found $numSymbols , required 0 <  < $MAX_NUM_SYMBOLS")
		require(maxIters > 0 && maxIters < MAX_NUM_ITERS, 
			s"HMMConfig: maxIters found $maxIters , required 0 <  < $MAX_NUM_ITERS")
		require(eps > 0 && eps < MAX_EPS, 
			s"HMMConfig: eps found $eps, required 0 <  < $MAX_EPS")
	}
}

// ----------------------------------------  EOF ------------------------------------------------------------
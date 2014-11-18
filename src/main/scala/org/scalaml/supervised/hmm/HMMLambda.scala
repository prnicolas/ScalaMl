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


import org.scalaml.core.types.ScalaMl._
import org.scalaml.util.Matrix
import scala.reflect.ClassTag
import scala.util.Random
import HMMConfig._

		/**
		 * <p>Class that defines the Lambda model (pi, A, B) for the HMM. The model is initialized with
		 * the state transition matrix, emission matrix and initial probabilities for the evaluation and 
		 * decoding canonical forms. These elements have to be computed using Baum_Welch for the training form.<br><br>
		 * <b>A</b>		State transition matrix<br>
		 * <b>B</b>		Observations	 or emission matrix<br>
		 * <b>pi</b>	Initial state probabilities</p>
		 * @constructor Create a Lambda model of type HMMLambda with a predefined state transition, emmision matrix and initial probabilities.
		 * @throws IllegalArgumentException if the number of observations, hidden states or symbols
		 * is out-of bounds
		 * 
		 * @author Patrick Nicolas
		 * @since March 6, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models/Hidden Markov Model - Evaluation
		 */
final protected class HMMLambda(val A: Matrix[Double], val B: Matrix[Double], var pi: DblVector, val numObs: Int) {

	@inline def getT: Int = numObs
	@inline def getN: Int = A.nRows
	@inline def getM: Int = B.nCols
	val d_1 = numObs-1
   
		/**
		 * <p>Initialize the Alpha value in the forward algorithm. Exceptions are caught
		 * by the client code.</p>
		 * @param obsSeqNum array of sequence number for the observations
		 * @throws IllegalArgumentException if obsSeqNum is undefined
		 */
	def initAlpha(obsSeqNum: Array[Int]): Matrix[Double] = {
		require( obsSeqNum != null && obsSeqNum.size > 0, "HMMLambda.initAlpha Cannot initialize HMM alpha with undefined obs sequence index")
  	
		Range(0, getN).foldLeft(Matrix[Double](getT, getN))((m, j) => {
			m += (0, j, alpha0(j, obsSeqNum(0)))
			m
		})
	}
  	
		/**
		 * <p>Update the value alpha by summation on a row of the transition matrix.</p>
		 * @param a value of the transition probability between state i and i +1
		 * @param i index of the column of the transition and emission probabilities matrix.
		 * @param obsIndex index in the observation in the sequence
		 * @return updated alpha value
		 * @throws IllegalArgumentException if index i or obsIndex are out of range.
		 */
	final def alpha(a: Double, i: Int, obsIdx: Int): Double = {
		require( i >= 0 && i < getN, s"HMMLambda.alpha Row index in transition and emission probabilities $i matrix is out of bounds")
		require( obsIdx >= 0, s"HMMLambda.alpha Col index in transition and emission probabilities $obsIdx  matrix is out of bounds")
  	  	 
		val sum = foldLeft(getN, (s, n) => s + a *A(i, n))
		sum*B(i, obsIdx) 
	}
  	 
 
	final def beta(b: Double, i: Int, lObs: Int): Double =  
		foldLeft(getN, (s, k) => s + b*A(i,k)*B(k, lObs))
  	   

		/**
		 * <p>Compute a new estimate of the log of the conditional probabilities for a given
		 * iteration. Arithmetic exception are caught by client code.</p>
		 * @param state Current state of the HMM execution 
		 * @param obs sequence of observations used in the estimate 
		 * @throws IllegalArgumentException if the HMM parameters are undefined or the sequence of observations is undefined.
		 */
	def estimate(state: HMMState, obs: Array[Int]): Unit = {
		require(state != null, "HMMLambda.estimate Cannot estimate the log likelihood of HMM with undefined parameters")
		require(obs != null && obs.size > 0, "HMMLambda.estimate Cannot estimate the log likelihood of HMM for undefined observations")
  	       
			// Recompute PI
		pi = Array.tabulate(getN)(i => state.Gamma(0, i) )
  	 
		foreach(getN, i => {	 
			// Recompute the state-transition matrix A
			var denominator = state.Gamma.fold(d_1, i)
			foreach(getN,  k => 
				A += (i, k, state.DiGamma.fold(d_1, i, k)/denominator)
			)
   
			// Recompute the observation emission matrix.
			denominator = state.Gamma.fold(getT, i)
			foreach(getM, k => 
				B += (i, k, state.Gamma.fold(getT, i, k, obs)/denominator)
			)
		})
	}
  
	
		/**
		 * <p>Normalize the state transition matrix A, emission matrix B and the
		 * initial probabilities pi</p>
		 */
	def normalize: Unit = {
		var min = A.data.min
		var delta = A.data.max - min
		Range(0, A.size).foreach(i => A.data.update(i, (A.data(i)-min)/delta) )

		min = B.data.min
		delta = B.data.max - min
		Range(0, B.size).foreach(i => B.data.update(i, (B.data(i)-min)/delta) )
     
		min = pi.min
		delta = pi.max - min
		Range(0, pi.size).foreach(i => pi.update(i, (pi(i)-min)/delta))
	}
  
  
	override def toString: String = {
		val piStr = pi.foldLeft(new StringBuilder)((b, x) => b.append(s"$x,"))
		s"A:${A.toString}\nB:${B.toString}\npi:${piStr}"
	}
  
	private def alpha0(j : Int, obsIndex: Int): Double = pi(j)*B(j, obsIndex)
}



		/**
		 * <p>Companion for the HMMLambda class to define the constructors apply.</p>
		 */
object HMMLambda {
	def apply(states: Seq[DblVector], symbols: Seq[DblVector]): HMMLambda = {
		require(states != null && states.size > 0, "Cannot create a HMM lambda model with states")
		require(symbols != null && symbols.size > 0, "Cannot create a HMM lambda model with states")

		val pi = Array.fill(states.size)(Random.nextDouble)
		
		// Square matrix A of the transition probabilities between states  (N states by N states)
		val A = Matrix[Double](states.size)
  	    
		// Matrix B of emission probabilities for N states and M symbols
		val B = Matrix[Double](states.size, symbols.size)

			// Load the states data to create the state transition matrix
		Range(0, states.size).foreach(i => {
			Range(0,  states.size).foreach(j => A += (i,j, states(i)(j)))
		})
  	  
			// Load the symbols
		Range(0, states.size).foreach(i => {
			Range(0, symbols.size).foreach(j => B += (i,j, symbols(i)(j)))
		})
		new HMMLambda(A, B, pi, states(0).size)
	}
	
	def apply(A: Matrix[Double], B: Matrix[Double], pi: DblVector, numObs: Int): HMMLambda = new HMMLambda(A, B, pi, numObs)
	
	def apply(config: HMMConfig): HMMLambda = {
		 val A = Matrix[Double](config._N)
		 A.fillRandom(0.0)
		
		 val B = Matrix[Double](config._N, config._M)
		 B.fillRandom(0.0)
		 
		 val pi = Array.fill(config._N)(Random.nextDouble)
		 new HMMLambda(A, B, pi, config._T)
	}
}


// ----------------------------------------  EOF ------------------------------------------------------------
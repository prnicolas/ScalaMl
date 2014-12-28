/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.supervised.hmm


import org.scalaml.core.Types.ScalaMl.{DblVector, DblMatrix}
import org.scalaml.core.Matrix
import org.scalaml.util.FormatUtils
import scala.reflect.ClassTag
import scala.util.Random
import HMMConfig._

		/**
		 * <p>Class that defines the Lambda model (pi, A, B) for the HMM. The model is initialized with
		 * the state transition matrix, emission matrix and initial probabilities for the evaluation and 
		 * decoding canonical forms. These elements have to be computed using Baum_Welch for the 
		 * training form.</p>
		 * @constructor Create a Lambda model of type HMMLambda with a predefined state transition, 
		 * Emission matrix and initial probabilities.
		 * @param A State transition probabilities matrix
		 * @param B Observations or emission probabilities matrix
		 * @param pi Initial state probabilities
		 * @throws IllegalArgumentException if the number of observations, hidden states or symbols
		 * is out-of bounds
		 * 
		 * @author Patrick Nicolas
		 * @since March 6, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models/Hidden Markov Model
		 */
final protected class HMMLambda(
		val A: Matrix[Double], 
		val B: Matrix[Double], 
		var pi: DblVector, 
		val numObs: Int) {

		/**
		 * Retrieve the number of sequential observations used in training
		 */
	@inline def getT: Int = numObs
	
		/**
		 * Retrieve the number of states for a sequence of observations
		 */
	@inline def getN: Int = A.nRows
	
		/**
		 * Retrieve the number of unique symbols (problem dimension) used in the sequence of observations.
		 */
	@inline def getM: Int = B.nCols

		/**
		 * Index of the last observation
		 */
	val d_1 = numObs-1
   
		/**
		 * <p>Initialize the Alpha value in the forward algorithm. Exceptions are caught
		 * by the client code.</p>
		 * @param obsSeqNum array of sequence number for the observations
		 * @return The matrix of alpha values.
		 * @throws IllegalArgumentException if obsSeqNum is undefined
		 */
	def initAlpha(obsSeqNum: Array[Int]): Matrix[Double] = {
		require( !obsSeqNum.isEmpty, 
				"HMMLambda.initAlpha Cannot initialize HMM alpha with undefined obs sequence index")
  	
		Range(0, getN).foldLeft(Matrix[Double](getT, getN))((m, j) => {
			m += (0, j, alpha0(j, obsSeqNum(0)))
			m
		})
	}
  	
		/**
		 * <p>Update the value alpha by summation on a row of the transition matrix.</p>
		 * @param a value of the transition probability between state i and i +1
		 * @param i index of the column of the transition and emission probabilities matrix.
		 * @param obsIndx index in the observation in the sequence
		 * @return updated alpha value
		 * @throws IllegalArgumentException if index i or obsIndex are out of range.
		 */
	final def alpha(a: Double, i: Int, obs: Int): Double = {
		require( i >= 0 && i < getN, 
				s"HMMLambda.alpha Row index $i in transition and emission  matrix is out of bounds")
		require( obs >= 0, 
				s"HMMLambda.alpha Observation $obs is out of bounds")
  	  	 
		val sum = foldLeft(getN, (s, n) => s + a *A(i, n))
		sum*B(i, obs) 
	}
  	 

		/**
		 * <p>Compute the Beta value for the current Lambda model
		 * @param current beta value
		 * @param i index of the state
		 * @param obs Index of the observation
		 * @return Update beta value
		 */
	final def beta(b: Double, i: Int, obs: Int): Double =  
		foldLeft(getN, (s, k) => s + b*A(i,k)*B(k, obs))
  	   

		/**
		 * <p>Compute a new estimate of the log of the conditional probabilities for a given
		 * iteration. Arithmetic exception are caught by client code.</p>
		 * @param state Current state of the HMM execution 
		 * @param obs sequence of observations used in the estimate 
		 * @throws IllegalArgumentException if the HMM parameters are undefined or the sequence 
		 * of observations is undefined.
		 */
	def estimate(state: HMMState, obs: Array[Int]): Unit = {
		require(state != null, 
				"HMMLambda.estimate Cannot estimate the log likelihood of HMM with undefined parameters")
		require(obs != null && obs.size > 0, 
				"HMMLambda.estimate Cannot estimate the log likelihood of HMM for undefined observations")
  	       
			// Recompute PI
		pi = Array.tabulate(getN)(i => state.Gamma(0, i) )
  	 
			// Traverse the list states of the HMM to update the 
			// transition probabilities matrix A and emission probabilities matrix B
		foreach(getN, i => {	 
			// Recompute/update  the state-transition matrix A
			var denominator = state.Gamma.fold(d_1, i)
			foreach(getN,  k => 
				A += (i, k, state.DiGamma.fold(d_1, i, k)/denominator)
			)
   
			// Recompute/update the observation emission matrix B using
			// the gamma coefficient
			denominator = state.Gamma.fold(getT, i)
			foreach(getM, k => 
				B += (i, k, state.Gamma.fold(getT, i, k, obs)/denominator)
			)
		})
	}
  
	
		/**
		 * <p>Normalize the state transition matrix A, emission matrix B and the
		 * initial probabilities pi. The normalization used the linear interpolation
		 * f(x) = (x - x_min)/x_max- x_min)</p>
		 */
	def normalize: Unit = {
			// Normalize the probabilities in the transition matrix A: 
			// a -> (a - a_min)/(a_max-a_min)
		var min = A.data.min
		var delta = A.data.max - min
		Range(0, A.size).foreach(i => A.data.update(i, (A.data(i)-min)/delta) )

			// Normalize the probabilities in the emission matrix B: 
			// b -> (b - b_min)/(b_max - b_min)
		min = B.data.min
		delta = B.data.max - min
		Range(0, B.size).foreach(i => B.data.update(i, (B.data(i)-min)/delta) )

			// Normalize the initial states probabilities PI as
			// (pi - pi_min)/(pi_max - pi_min)
		min = pi.min
		delta = pi.max - min
		Range(0, pi.size).foreach(i => pi.update(i, (pi(i)-min)/delta))
	}
  
		/**
		 * Textual representation of the Lambda (A, B, pi) model.
		 */
	override def toString: String = {
		val piStr = pi.foldLeft(new StringBuilder)((b, x) => 
				b.append(s"${FormatUtils.format(x,"", FormatUtils.ShortFormat)}") )
				
		s"State transition probs A\n${A.toString}\nEmission probs B\n${B.toString}\nInitial probs pi\n${piStr}"
	}
 
		/*
		 * Compute alpha for the observation t = 0. (Formula M1)
		 */
	private def alpha0(j : Int, obsIndex: Int): Double = pi(j)*B(j, obsIndex)
}

		/**
		 * <p>Companion for the HMMLambda class to define the constructors of the class HMMLambda.</p>
		 * @author Patrick Nicolas
		 * @since March 6, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models/Hidden Markov Model 
		 */
object HMMLambda {
		
		/**
		 * <p>Constructor for the HMMLambda model of HMM. This constructor takes
		 * the value of states and the value of symbols. This constructor 
		 * can be invoked for the evaluation and decoding form of HMM 
		 * once trained.</p>
		 * @param states value of the states (as a sequence of floating point values)
		 * @param symbols values of the symbols  (as a sequence of floating point values)
		 */
	def apply(states: Seq[DblVector], symbols: Seq[DblVector]): HMMLambda = {
		require(states != null && states.size > 0, 
				"Cannot create a HMM lambda model with underfined states")
		require(symbols != null && symbols.size > 0, 
				"Cannot create a HMM lambda model with undefined symbol values")

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
	
		/**
		 * Default constructor for the HMMLambda model
		 * @param A State transition probabilities matrix
		 * @param B Observations or emission probabilities matrix
		 * @param pi Initial state probabilities
		 * @param numObs number of observed stated
		 */
	def apply(
			A: Matrix[Double], 
			B: Matrix[Double], 
			pi: DblVector, 
			numObs: Int): HMMLambda = new HMMLambda(A, B, pi, numObs)
	
		/**
		 * <p>Constructor for the training canonical form of the HMM (Baum Welch).
		 * The transition probabilities and emission probabilities are initialized
		 * as uniform random value [0, 1].</p>
		 * @param config Configuration for the HMM to generate an initial Lambda model
		 */
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
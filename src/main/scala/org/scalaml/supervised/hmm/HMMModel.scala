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


import scala.reflect.ClassTag
import scala.util.Random

import org.scalaml.core.Types.ScalaMl.{DblArray, DblMatrix}
import org.scalaml.core.Types.emptyString
import org.scalaml.util.MathUtils._
import org.scalaml.supervised.hmm.train.{Gamma, DiGamma}
import org.scalaml.util.FormatUtils._
import HMMConfig._


		/**
		 * Define the result of the prediction (decoding or evaluation) as a
		 * a tuple of (likelihood, sequence (array) of observations indexes).
		 * @param likelihood Likelihood associated for the prediction
		 * @param states Optimum sequence of states for this prediction
		 */
case class HMMPrediction(likelihood: Double, states: Array[Int]) {
  override def toString: String = s"Likelihood: $likelihood, States:${states.mkString(",")}"
}

	

		/**
		 * Class that defines the Lambda model (pi, A, B) for the HMM. The model is initialized with
		 * the state transition matrix, emission matrix and initial probabilities for the evaluation and 
		 * decoding canonical forms. These elements have to be computed using Baum_Welch for the 
		 * training form.
		 * @constructor Create a Lambda model of type HMMModel with a predefined state transition, 
		 * Emission matrix and initial probabilities.
		 * @param A State transition probabilities matrix
		 * @param B Observations or emission probabilities matrix
		 * @param pi Initial state probabilities
		 * @param numObs Number of observations
		 * @throws IllegalArgumentException if the number of observations, hidden states or symbols
		 * is out-of bounds
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98 March 6, 2014
		 * @version 0.99.1
		 * @see Scala for Machine Learning Chapter 7 ''Sequential data models'' / Hidden Markov Model
		 */
final protected class HMMModel(
		val A: DMatrix, 
		val B: DMatrix, 
		var pi: DblArray, 
		val numObs: Int) {
	
		/**
		 * Retrieve the number of states for a sequence of observations
		 */
	val numStates = A.nRows
	
		/**
		 * Retrieve the number of unique symbols (problem dimension) used in the sequence of 
		 * observations.
		 */
	val numSymbols = B.nCols

  
		/**
		 * Initialize the Alpha value in the forward algorithm. Exceptions are caught
		 * by the client code.
		 * @param obsSeq array of sequence number for the observations
		 * @return The matrix of alpha values.
		 * @throws IllegalArgumentException if obsSeqNum is undefined
		 */
	def setAlpha(obsSeq: Vector[Int]): DMatrix = {
		require( obsSeq.length > 1, 
				"HMMModel.setAlpha Cannot initialize HMM alpha with undefined obs sequence index")

		Range(0, numStates)./:(DMatrix(numObs, numStates))((m, j) => 
			m += (0, j, pi(j)*B(j, obsSeq.head)))
	}
  	

	
		/**
		 * Update the value alpha by summation on a row of the transition matrix.
		 * @param a value of the transition probability between state i and i +1
		 * @param i index of the column of the transition and emission probabilities matrix.
		 * @param obsId index in the observation in the sequence
		 * @return updated alpha value
		 * @throws IllegalArgumentException if index i or obsIndex are out of range.
		 */
	final def getAlphaVal(a: Double, i: Int, obsId: Int): Double = {
		require( i >= 0 && i < numStates, 
				s"HMMModel.alpha Row index $i in transition and emission  matrix is out of bounds")
		require( obsId >= 0, 
				s"HMMModel.alpha Observation $obsId is out of bounds")
  	  	 
		val sum = /:(numStates, (s, n) => s + a *A(i, n))
		sum*B(i, obsId) 
	}

		/**
		 * Validate the state transition, emission matrices and the vector of initial probabilities
		 * extracted from training. The test consists of verifying that the sum of probabilities
		 * for each row is equal to 1
		 * @param eps Error tolerance value used in validating the model components A, B and pi
		 * @return true is the matrices A, B and the vector pi are normalized as probabilities
		 */
	final def validate(eps: Double): Boolean = 
		!(Range(0, A.nRows).exists(n => Math.abs(A.row(n).sum - 1.0) > eps) ||
		Range(0, B.nRows).exists(n => Math.abs(B.row(n).sum - 1.0) > eps) ||
		Math.abs(pi.sum -1.0) > eps)
	

		/**
		 * Compute the Beta value for the current Lambda model
		 * @param b current beta value
		 * @param i index of the state
		 * @param obsId Index of the observation
		 * @return Update beta value
		 */
	final def getBetaVal(b: Double, i: Int, obsId: Int): Double =  
		/:(numStates, (s, k) => s + b*A(i,k)*B(k, obsId))

		/**
		 * Compute a new estimate of the log of the conditional probabilities for a given
		 * iteration. Arithmetic exception are caught by client code.
		 * @param gamma Current values for the gamma matrix
		 * @param diGamma current values for the array of DiGamma matrices
		 * @param obsSeq sequence of observations used in the estimate 
		 * @throws IllegalArgumentException if the HMM parameters are undefined or the sequence 
		 * of observations is undefined.
		 */
	def update(gamma: Gamma, diGamma: DiGamma, obsSeq: Vector[Int]): Unit = {
		require(obsSeq.length > 1, 
				"HMMModel.estimate Cannot estimate the log likelihood of HMM for undefined observations")

		val dim_1 = numObs -1
			// Recompute PI
		pi = Array.tabulate(numStates)(i => gamma(0,i) )
  	 
			// Traverse the list states of the HMM to update the 
			// transition probabilities matrix A and emission probabilities matrix B
		foreach(numStates, i => {	 
			// Recompute/update  the state-transition matrix A
			var denominator = gamma.fold(dim_1, i)
			foreach(numStates, k => 
				A += (i, k, diGamma.fold(dim_1, i, k)/denominator)
			)
			
			// Recompute/update the observation emission matrix B using
			// the gamma coefficient
			denominator = gamma.fold(numObs, i)

			foreach(numSymbols, k => 
				B += (i, k, gamma.fold(numObs, i, k, obsSeq.toArray)/denominator)
			)
		})
	}
  
	
		/**
		 * Normalize the state transition matrix A, emission matrix B and the
		 * initial probabilities pi. The normalization consists of computing the sum
		 * of values of each row and divided each values by that sum.
		 * f(x) = x/SUM{x}
		 */
	def normalize(): Unit = {
		A.normalizeRows()
		B.normalizeRows()

		val sum = pi.sum
		pi = pi.map( _ /sum)
	}
  
		/**
		 * Textual representation of the Lambda (A, B, pi) model.
		 */
	override def toString: String = {
		val piStr = pi.map(x => s"${format(x, emptyString, SHORT)}").mkString(", ")
		s"State transition A\n${A.toString}\nEmission B\n${B.toString}\nInitial pi\n$piStr"
	}
 
		/*
		 * Compute alpha for the observation t = 0. (Formula M1)
		 */
	private def alpha0(j : Int, obsIndex: Int): Double = pi(j)*B(j, obsIndex)

}

		/**
		 * Companion for the HMMModel class to define the constructors of the class HMMModel.
		 * @author Patrick Nicolas
		 * @since March 6, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models/Hidden Markov Model 
		 */
object HMMModel {
		
		/**
		 * Constructor for the HMMModel model of HMM. This constructor takes
		 * the value of states and the value of symbols. This constructor 
		 * can be invoked for the evaluation and decoding form of HMM 
		 * once trained.
		 * @param statesSeq Sequence of values of the states (as a sequence of floating point values)
		 * @param symbolsSeq Sequence of the values of the symbols  (as a sequence of floating 
		 * point values)
		 */
	def apply(statesSeq: Seq[DblArray], symbolsSeq: Seq[DblArray]): HMMModel = {
		require(statesSeq.nonEmpty,
				"Cannot create a HMM lambda model with undefined states")
		require(symbolsSeq.nonEmpty,
				"Cannot create a HMM lambda model with undefined symbol values")

		val pi = Array.fill(statesSeq.size)(Random.nextDouble())
		
		// Square matrix A of the transition probabilities between states  (N states by N states)
		val A = DMatrix.fill(statesSeq.size)(statesSeq)
		val B =  DMatrix.fill(statesSeq.size)(symbolsSeq)
		new HMMModel(A, B, pi, statesSeq.head.length)
	}
	
		/**
		 * Default constructor for the HMMModel model
		 * @param A State transition probabilities matrix
		 * @param B Observations or emission probabilities matrix
		 * @param pi Initial state probabilities
		 * @param numObs number of observed stated
		 */
	def apply(
			A: DMatrix, 
			B: DMatrix, 
			pi: DblArray, 
			numObs: Int): HMMModel = new HMMModel(A, B, pi, numObs)
	
		/**
		 * Constructor for the training canonical form of the HMM (Baum Welch).
		 * The transition probabilities and emission probabilities are initialized
		 * as uniform random value [0, 1].
		 * @param config Configuration for the HMM to generate an initial Lambda model
		 * @return HMMModel insgtance
		 */
	def apply(config: HMMConfig): HMMModel = {
		 val A = DMatrix(config.numStates, config.numStates, 0.0)
		 val B = DMatrix(config.numStates, config.numSymbols, 0.0)
		 val pi = Array.fill(config.numStates)(Random.nextDouble())
		 
		 new HMMModel(A, B, pi, config.numObs)
	}
	
		/**
		 * Validate the dimension of the lambda matrices (state transition, emission) and
		 * the array of initial probabilities
		 * @param A probabilities state transition matrix
		 * @param B probabilities emission matrix
		 * @param pi initial state probabilities
		 * @return true if the dimension of A, B and pi are correct, false otherwise
		 */
	def validate(A: DblMatrix, B: DblMatrix, pi: DblArray): Boolean = 
		A.length == A.head.length && 
		B.length == A.length && 
		pi.length == A.length
		
	
		/**
		 * Validate the dimension of the lambda matrices (state transition, emission) and
		 * the array of initial probabilities
		 * @param A probabilities state transition matrix
		 * @param B probabilities emission matrix
		 * @param pi initial state probabilities
		 * @return true if the dimension of A, B and pi are correct, false otherwise
		 */
	def validate(A: DMatrix, B: DMatrix, pi: DblArray): Boolean = 
		A.nRows == A.nCols && B.nCols == A.nCols && pi.length == A.nRows
		
				/**
		 * Validate the dimension of the lambda matrices (state transition, emission) and
		 * the array of initial probabilities
		 * @param model lambda model
		 * @return true if the dimension of the lambda model components are correct, false otherwise
		 */
	def validate(model: HMMModel): Boolean = 
		model.A.nRows == model.A.nCols && 
		model.B.nCols == model.A.nCols && 
		model.pi.length == model.A.nRows
}


// ----------------------------------------  EOF ------------------------------------------------------------
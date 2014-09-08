/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.supervised.hmm


import org.scalaml.core.Types
import org.scalaml.util.Matrix
import Types.ScalaMl._
import scala.reflect.ClassTag
import scala.util.Random


	/**
	 * <p>Class that defines the Lambda model (pi, A, B) for the HMM.</p>
	 * @param _T number of observations used in the training of the HMM
	 * @param _N number of hidden states in the HMM model
	 * @param _M number of symbols (or labels) in the HMM
	 * @throws IllegalArgumentException if the number of observations, hidden states or symbols
	 * is out-of bounds
	 * @author Patrick Nicolas
	 * @since March 6, 2014
	 */
final class HMMLambda(val states: Seq[Array[Double]], val symbols: Seq[Array[Double]]) {
  require(states != null && states.size > 0, "Cannot create a HMM lambda model with states")
  require(symbols != null && symbols.size > 0, "Cannot create a HMM lambda model with states")
    
  val config = new HMMConfig(states.size, states(0).size, symbols(0).size)
      
  	/**
  	 * Vector of hidden states length for the initial probability of the sequence
  	 */
  var pi: DblVector = _
  
     /**
  	 * Square matrix A of the transition probabilities between states  (N states by N states)
  	 */
  val A = Matrix[Double](config._N)
  	    
  
  	/**
  	 * Matrix B of emission probabilities for N states and M symbols
  	 */
  val B = Matrix[Double](config._N, config._M)
  
  	// Last observation index
  val d_1 = config._T-1
  
  load

  
  private def alpha0(j : Int, obsIndex: Int): Double = pi(j)*B(j, obsIndex)
  
  	/**
  	 * <p>Initialize the Alpha value in the forward algorithm. Exceptions are caught
  	 * by the client code.</p>
  	 * @param obsSeqNum array of sequence number for the observations
  	 * @throws IllegalArgumentException if obsSeqNum is undefined
  	 */
  def initAlpha(obsSeqNum: Array[Int]): Matrix[Double] = {
  	require( obsSeqNum != null && obsSeqNum.size > 0, "Cannot initialize HMM alpha with undefined obs sequence index")
  	
  	Range(0, config._N).foldLeft(Matrix[Double](config._M, config._N))((m, j) => {
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
  def alpha(a: Double, i: Int, obsIdx: Int): Double = {
  	 require( i >= 0 && i < config._N, "Row index in transition and emission probabilities " + i + " matrix is out of bounds")
  	 require( obsIdx >= 0 && i < config._M, "Row index in transition and emission probabilities " + obsIdx + " matrix is out of bounds")
  	  	 
  	 HMMConfig.foldLeft(config._N, (s, n) => s + a *A(i, n))*B(i, obsIdx) 
  }
  	 
 
  def beta(b: Double, i: Int, lObs: Int): Double =  
  	 HMMConfig.foldLeft(config._N, (s, k) => s + b*A(i,k)*B(k, lObs))
  	   

  	 /**
  	  * <p>Compute a new estimate of the log of the conditional probabilities for a given
  	  * iteration. Arithmetic exception are caught by client code.</p>
  	  * @param params HMM parameters 
  	  * @param obs sequence of observations used in the estimate 
  	  * @throws IllegalArgument if the HMM parameters are undefined or the sequence of observatiosn is undefined.
  	  */
  def estimate(state: HMMState, obsIdx: Array[Int]): Unit = {
  	 require(state != null, "Cannot estimate the log likelihood of HMM with undefined parameters")
  	 require(obsIdx != null && obsIdx.size > 0, "Cannot estimate the log likelihood of HMM for undefined observations")
  	       
  	 pi = Array.tabulate(config._N)(i => state.Gamma(0, i) )
     HMMConfig.foreach(config._N, i => {
    	 var denominator = state.Gamma.fold(d_1, i)
    	 HMMConfig.foreach(config._N,  k => 
    		  A += (i, k, state.DiGamma.fold(d_1, i, k)/denominator)
         )
   
    	 denominator = state.Gamma.fold(config._T, i)
    	 HMMConfig.foreach(config._N, k => B += (i, k, state.Gamma.fold(config._T, i, k, obsIdx)/denominator))
     })
  }
  
  private def load: Unit = {
  		// Load the states data to create the state transition matrix
  	  Range(0, config._N).foreach(i => {
  	  	 Range(0, config._N).foreach(j => A += (i,j, states(i)(j)))
  	  })
  	  
  	  	// Load the symbols
  	  Range(0, config._N).foreach(i => {
  	  	 Range(0, config._M).foreach(j => B += (i,j, symbols(i)(j)))
  	  })
  	  
  	  pi = Array.fill(config._N)(Random.nextDouble)
  }
}



		/**
		 * <p>Companion for the HMMLambda class to define the constructors apply.</p>
		 */
object HMMLambda {
	def apply(states: Seq[Array[Double]], symbols: Seq[Array[Double]]): HMMLambda = new HMMLambda(states, symbols)
}


// ----------------------------------------  EOF ------------------------------------------------------------
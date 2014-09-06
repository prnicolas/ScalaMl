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
final class HMMLambda(val dim: HMMDim) {
  require(dim != null, "Cannot create a HMM lambda model with undefined dimension")
      
  	/**
  	 * Vector of hidden states length for the initial probability of the sequence
  	 */
  var pi = new DblVector(dim._N)
  
     /**
  	 * Square matrix A of the transition probabilities between states  (N states by N states)
  	 */
  val A = Matrix[Double](dim._N)
  
  	/**
  	 * Matrix B of emission probabilities for N states and M symbols
  	 */
  val B = Matrix[Double](dim._N, dim._M)
  
  	// Last observation index
  val d_1 = dim._T-1

  
  private def alpha0(j : Int, initialObs: Int): Double = pi(j)*B(j, initialObs)
  
  	/**
  	 * <p>Initialize the Alpha value in the forward algorithm. Exceptions are caught
  	 * by the client code.</p>
  	 * @param obsSeqNum array of sequence number for the observations
  	 * @throws IllegalArgumentException if obsSeqNum is undefined
  	 */
  def initAlpha(obsSeqNum: Array[Int]): Matrix[Double] = {
  	require( obsSeqNum != null && obsSeqNum.size > 0, "Cannot initialize HMM alpha with undefined obs sequence index")
  	
  	Range(0, dim._N).foldLeft(Matrix[Double](dim._M, dim._N))((m, j) => {
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
  def alpha(a: Double, i: Int, obsIndex: Int): Double = {
  	 require( i >= 0 && i < dim._N, "Row index in transition and emission probabilities " + i + " matrix is out of bounds")
  	 require( obsIndex >= 0 && i < dim._M, "Row index in transition and emission probabilities " + obsIndex + " matrix is out of bounds")
  	  	 
  	 HMMDim.foldLeft(dim._N, (s, n) => s + a *A(i, n))*B(i, obsIndex) 
  }
  	 
 
  def beta(b: Double, i: Int, lObs: Int): Double =  
  	HMMDim.foldLeft(dim._N, (s, k) => s + b*A(i,k)*B(k, lObs))
  	   

  	 /**
  	  * <p>Compute a new estimate of the log of the conditional probabilities for a given
  	  * iteration. Arithmetic exception are caught by client code.</p>
  	  * @param params HMM parameters 
  	  * @param obs sequence of observations used in the estimate 
  	  * @throws IllegalArgument if the HMM parameters are undefined or the sequence of observatiosn is undefined.
  	  */
  def estimate(config: HMMConfig, obs: Array[Int]): Unit = {
  	 require(config != null, "Cannot estimate the log likelihood of HMM with undefined parameters")
  	 require(obs != null && obs.size > 0, "Cannot estimate the log likelihood of HMM for undefined observations")
  	       
  	 pi = Array.tabulate(dim._N)(i => config.Gamma(0, i) )
     HMMDim.foreach(dim._N, i => {
    	 var denominator = config.Gamma.fold(d_1, i)
    	 HMMDim.foreach(dim._N,  k => 
    		  A += (i, k, config.DiGamma.fold(d_1, i, k)/denominator)
         )
   
    	 denominator = config.Gamma.fold(dim._T, i)
    	 HMMDim.foreach( dim._N, k => B += (i, k, config.Gamma.fold(dim._T, i, k, obs)/denominator))
     })
  }
}



		/**
		 * <p>Companion for the HMMLambda class to define the constructors apply.</p>
		 */
object HMMLambda {
	def apply(dim: HMMDim): HMMLambda = new HMMLambda(dim)
	def apply(_T: Int, pi: DblVector, A: Matrix[Double], B: Matrix[Double]) = new HMMLambda(new HMMDim(_T, pi.size, B.nCols))
}


// ----------------------------------------  EOF ------------------------------------------------------------
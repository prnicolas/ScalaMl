/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 * 
 */
package org.scalaml.supervised.hmm


import org.scalaml.core.Types
import org.scalaml.util.Matrix
import Types._
import scala.reflect.ClassTag


	/**
	 * <p>Class that encapsulates the execution parameters for the three
	 * canonical forms of the HMM.</p>
	 * @param d dimension for the HMM
	 * @param maxIters maximum number of iterations used in the training of HMM
	 * @exception if range is undefined or the maximum iterations is out of range
	 */
final class HMMParams(val d: HMMDim, val maxIters: Int) {
  require(d != null, "Cannot configure a HMM with undefined dimension")
  require( maxIters > 0 &&  maxIters < 1000, "Maximum number of iterations " + maxIters + " is out of range")
  
  val alpha = Matrix[Double](d._T, d._N)
  val beta = Matrix[Double](d._T, d._N)
  val delta = Matrix[Double](d._T, d._N)
  val psi = Matrix[Int](d._T, d._N)
  
  object QStar {
  	private val qStar = Array.fill(d._T)(0)
  	
  	def update(t: Int, index: Int): Unit = {
  	   qStar(t-1) = index
  	   (t-2 to 0 by -1).foreach( s => {qStar(s) = psi(s+1, qStar(s+1)) })
  	}
  	def apply(): Array[Int] = qStar
  }
  
  object DiGamma {
  	private val diGamma = Array.fill(d._T-1)(Matrix[Double](d._N, d._N))
  	
  	def update(A: Matrix[Double], B: Matrix[Double], obs: Array[Int]): Unit = {
       try {
      	  val rn1 = Range(0, d._N+1)
	       (0 until d._T-1).foreach( t => {     	
	           val sum =  d.rn.foldLeft(0.0)( (sst, i) => {
	               sst + rn1.foldLeft(0.0)( (s, j) => {
	            	  diGamma(t) += (i, j, A(t,i)*beta(t+1, i)* A(i,j)*B(j, obs(t+1)))
	            	  s + diGamma(t)(i, j)
	               })
	           })
	           
	           d.rt.foreach( i => {
	              rn1.foreach(j => 
	                 diGamma(t) += (i, j,  diGamma(t)(i,j)/sum) )
	            })
	        })
       }
       catch {
      	 case e: ArithmeticException => Console.println(e.toString)
       }
  	 }
  	
  	def fold(t: Int, i: Int, j: Int): Double = (0 until t).foldLeft(0.0)((s, k) => s + diGamma(k)(i,j) )
  }
  
  
  
  object Gamma {
  	private[this] val values = {
  	   val gamma = Matrix[Double](d._T, d._N)

  	   d.rt.foreach( t => {
  	      val sum = d.rn.foldLeft(0.0)((s, i) => {gamma += (t, i, alpha(t,i)*beta(t,i));  s + gamma(t,i) } )
  	      gamma.cols(t).map( _ / sum)
  	   })
  	   gamma
  	}
  	
  	def fold(t: Int, i: Int): Double = (0 until t).foldLeft(0.0)((s, n) => s + values(n, i))
  	def fold(t: Int, i: Int, k: Int, obs: Array[Int]): Double = (0 until t).foldLeft(0.0)((s, n) => s + { if(obs(n) ==k) values(n, i) else 0.0} )
  	
  	def apply(i: Int, j: Int): Double = values(i,j)
  }
}


object HMMParams {
	final val DEFAULT_MAXITERS = 20
	def apply(d: HMMDim, maxIters: Int): HMMParams = new HMMParams(d, maxIters)
	def apply(d: HMMDim): HMMParams = new HMMParams(d, DEFAULT_MAXITERS)
}

// ----------------------------------------  EOF ------------------------------------------------------------
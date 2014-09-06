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
import org.scalaml.supervised.Config
import Types._
import scala.reflect.ClassTag


	/**
	 * <p>Class that encapsulates the execution parameters for the three
	 * canonical forms of the HMM.</p>
	 * @param d dimension for the HMM
	 * @param maxIters maximum number of iterations used in the training of HMM
	 * @throws if range is undefined or the maximum iterations is out of range
	 */
final class HMMConfig(val dim: HMMDim, val maxIters: Int) extends Config {
  require(dim != null, "Cannot configure a HMM with undefined dimension")
  require( maxIters > 0 &&  maxIters < 1000, "Maximum number of iterations " + maxIters + " is out of range")
  
  val alpha = Matrix[Double](dim._T, dim._N)
  val beta = Matrix[Double](dim._T, dim._N)
  val delta = Matrix[Double](dim._T, dim._N)
  val psi = Matrix[Int](dim._T, dim._N)
  
  object QStar {
  	private val qStar = Array.fill(dim._T)(0)
  	
  	def update(t: Int, index: Int): Unit = {
  	   qStar(t-1) = index
  	   (t-2 to 0 by -1).foreach( s => {qStar(s) = psi(s+1, qStar(s+1)) })
  	}
  	def apply(): Array[Int] = qStar
  }
  
  object DiGamma {
  	private val diGamma = Array.fill(dim._T-1)(Matrix[Double](dim._N, dim._N))
  	
  	def update(A: Matrix[Double], B: Matrix[Double], obs: Array[Int]): Unit = {
       try {
	      HMMDim.foreach(dim._T-1, t => {     	
	           val sum =  HMMDim.foldLeft(dim._N, (sst, i) => {
	               sst + HMMDim.foldLeft(dim._N+1, (s, j) => {
	            	  diGamma(t) += (i, j, A(t,i)*beta(t+1, i)* A(i,j)*B(j, obs(t+1)))
	            	  s + diGamma(t)(i, j)
	               })
	           })
	            
	           HMMDim.foreach(dim._T, i => {
	              HMMDim.foreach(dim._N+1, j => 
	                 diGamma(t) += (i, j,  diGamma(t)(i,j)/sum) )
	            })
	        })
       }
       catch {
      	 case e: ArithmeticException => Console.println(e.toString)
       }
  	 }
  	
  	def fold(t: Int, i: Int, j: Int): Double = HMMDim.foldLeft(t, (s, k) => s + diGamma(k)(i,j) )
  }
  
  
  
  object Gamma {
  	private[this] val values = {
  	   val gamma = Matrix[Double](dim._T, dim._N)

  	   HMMDim.foreach(dim._T, t => {
  	      val sum = HMMDim.foldLeft(dim._N, (s, i) => {
  	      	 gamma += (t, i, alpha(t,i)*beta(t,i))
  	      	 s + gamma(t,i) 
  	      })
  	      gamma.cols(t).map( _ / sum)
  	   })
  	   gamma
  	}
  	
  	def fold(t: Int, i: Int): Double = HMMDim.foldLeft(t, (s, n) => s + values(n, i))
  	def fold(t: Int, i: Int, k: Int, obs: Array[Int]): Double = 
  		 HMMDim.foldLeft(t, (s, n) => s + { if(obs(n) ==k) values(n, i) else 0.0} )
  	 
  	
  	def apply(i: Int, j: Int): Double = values(i,j)
  }
}


object HMMConfig {
	final val DEFAULT_MAXITERS = 20
	def apply(dim: HMMDim, maxIters: Int): HMMConfig = new HMMConfig(dim, maxIters)
	def apply(dim: HMMDim): HMMConfig = new HMMConfig(dim, DEFAULT_MAXITERS)
}

// ----------------------------------------  EOF ------------------------------------------------------------
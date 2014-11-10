/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 * 
 * Version 0.95c
 */
package org.scalaml.supervised.hmm


import org.scalaml.util.Matrix
import scala.reflect.ClassTag
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display
import HMMConfig._

	/**
	 * <p>Class that encapsulates the execution parameters for the three
	 * canonical forms of the HMM.</p>
	 * @param d configension for the HMM
	 * @param maxIters maximum number of iterations used in the training of HMM
	 * @throws if range is undefined or the maximum iterations is out of range
	 * 
	 * @author Patrick Nicolas
	 * @since March 24, 2014
	 * @note Scala for Machine Learning Chapter 7 Sequential data models/Hidden Markov Model
	 */
final class HMMState(val lambda: HMMLambda, val maxIters: Int) {
  require(lambda != null, "Cannot initialize the state of the computation of the HMM with undefined model")
  require( maxIters > 0 &&  maxIters < 1000, s"Maximum number of iterations $maxIters is out of range")
  
  private val logger = Logger.getLogger("HMMState")

  val delta = Matrix[Double](lambda.getT, lambda.getN)
  val psi = Matrix[Int](lambda.getT, lambda.getN)
  
  object QStar {
  	private val qStar = Array.fill(lambda.getT)(0)
  	
  	def update(t: Int, index: Int): Unit = {
  	   qStar(t-1) = index
  	   (t-2 to 0 by -1).foreach( s => {qStar(s) = psi(s+1, qStar(s+1)) })
  	}
  	def apply(): Array[Int] = qStar
  }
  
  
  def update(alpha: Matrix[Double], beta: Matrix[Double], A: Matrix[Double], B: Matrix[Double], obs: Array[Int]): Int= {
  	 Gamma.update(alpha, beta)
  	 DiGamma.update(alpha, beta, A, B, obs)
  }
  
  object DiGamma {
  	private val diGamma = Array.fill(lambda.getT-1)(Matrix[Double](lambda.getN, lambda.getN))
  	
  	def update(alpha: Matrix[Double], beta: Matrix[Double], A: Matrix[Double], B: Matrix[Double], obs: Array[Int]): Int = {
       Try {
	      foreach(lambda.getT-1, t => {     	
	           val sum =  foldLeft(lambda.getN, (sst, i) => {
	               sst + foldLeft(lambda.getN, (s, j) => {
	            	  diGamma(t) += (i, j, alpha(t,i)*beta(t+1, i)* A(i,j)*B(j, obs(t+1)))
	            	  s + diGamma(t)(i, j)
	               })
	           })
	            
	           foreach(lambda.getN, i => {
	              foreach(lambda.getN, j => 
	                 diGamma(t) += (i, j,  diGamma(t)(i,j)/sum) )
	            })
	        })
	        1
       } match {
          case Success(n) => n
          case Failure(e) => Display.error("HMMState.DiGamma", logger, e)
       }
  	 }
  	
  	def fold(t: Int, i: Int, j: Int): Double = foldLeft(t, (s, k) => s + diGamma(k)(i,j) )
  }
  
  
  
  object Gamma {
  	private[this] val values: Matrix[Double] = Matrix[Double](lambda.getT, lambda.getN)
  	
  	def update(alpha: Matrix[Double], beta: Matrix[Double]): Unit = {
  	   foreach(lambda.getT, t => {
  	      val sum = foldLeft(lambda.getN, (s, i) => {
  	      	 values += (t, i, alpha(t,i)*beta(t,i))
  	      	 s + values(t,i) 
  	      })
  	      values.cols(t).map( _ / sum)
  	   })
  	}
  	
  	def fold(t: Int, i: Int): Double = foldLeft(t, (s, n) => s + values(n, i))
  	def fold(t: Int, i: Int, k: Int, obs: Array[Int]): Double = 
  		 foldLeft(t, (s, n) => s + { if(obs(n) ==k) values(n, i) else 0.0} )
  	 
  	def apply(i: Int, j: Int): Double = values(i,j)
  }
}


object HMMState {
	final val DEFAULT_MAXITERS = 20
	def apply(lambda: HMMLambda, maxIters: Int): HMMState = new HMMState(lambda, maxIters)
	def apply(lambda: HMMLambda): HMMState = new HMMState(lambda, DEFAULT_MAXITERS)
}

// ----------------------------------------  EOF ------------------------------------------------------------
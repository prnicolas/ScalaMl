/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap7

import org.scalaml.util.Matrix
import org.scalaml.supervised.hmm.{HMM, HMMForm, HMMLambda}


		/**
		 * Singleton for the evaluation of Hidden Markov Models presented in chapter 7
		 * 
		 * @author Patrick Nicolas
		 * @since March 28, 2014
		 * @note Scala for Machine Learning
		 */
object HMMEval {
  def run: Unit = {
  	 Console.println("Evaluation of Hidden Markov Models")
  	 import HMM._, HMMForm._
   
     val pi = Array[Double](0.6, 0.4)
     val A = Matrix[Double](2, 2)
     A += (0, 0, 0.7)
     A += (0, 1, 0.3)
     A += (1, 0, 0.4)
     A += (1, 1, 0.6)
   
     val B = Matrix[Double](2, 3)
     B += (0, 0, 0.1)
     B += (0, 1, 0.4)
     B += (0, 2, 0.5)
     B += (1, 0, 0.7)
     B += (1, 1, 0.2)
     B += (1, 2, 0.1)
   
    Range(0, 2).foreach( i => Range(0, 3).foreach(k =>  println("(" + i + "," + k + ") =" + B(i,k) )))
   
    val lambda = HMMLambda(4, pi, A, B)
    val hmm = HMM[Array[Int]](lambda, EVALUATION)
  }
}



// --------------------------------  EOF -------------------------------
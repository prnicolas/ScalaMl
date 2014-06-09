/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap6

import org.scalaml.util.Matrix
import org.scalaml.supervised.hmm.{HMM, HMMForm, HMMLambda}
import org.scalaml.supervised.crf.{CrfConfig,  CrfSeqDelimiter, CrfLinearChain}
import java.io.IOException


object CRFTest {
  def test: Unit = {
    Console.println("CRFTest.test")
    
	val l2Penalty = 0.5
	val nLabels = 9
	val config = CrfConfig(0.7, 100, l2Penalty, 0.01)
	val delimiters = CrfSeqDelimiter(",\t/ -():.;'?#`&_", "//", "\n")
	    
	try {
	   val crf = new CrfLinearChain(nLabels, config, delimiters, "resources/data/chap6/rating")
	   val lambdas = crf.lambdas
	   if( lambdas == null)
		   println("Count not train the CRF model")
	   else
		   lambdas.foreach( println )
	 }
     catch {
		case e: IllegalArgumentException => Console.println(e.toString)
		case e: IOException => Console.println(e.toString)
	 }
  }
}


object HMMTest {
  def test: Unit = {
  	 Console.println("HMMTest.test")
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
   
    for( i <- 0 until 2) {
  	   for (k <- 0 until 3) 
  		 println("(" + i + "," + k + ") =" + B(i,k) )
    }  
   
    val lambda = HMMLambda(4, pi, A, B)
    val hmm = HMM[Array[Int]](lambda, EVALUATION)
  }
}




object Chap6 extends App {
   if( args == null)
  	 Console.println("Program arguments missing: Chap6 [HNN | CRF]" )
   else {
  	 args(1) match {
  		 case "HMM" => HMMTest.test
  		 case "CRF" => CRFTest.test
  		 case _ => Console.println("Incorrect program argument " + args(1))
  	 }
   }
}

// --------------------------------  EOF -------------------------------
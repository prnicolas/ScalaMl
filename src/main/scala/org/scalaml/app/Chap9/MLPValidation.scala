/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap9


import org.scalaml.supervised.nnet._
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import scala.util.Random
import org.scalaml.supervised.nnet.MLPConnection.setNoSoftmax


import MLP._
object MLPValidation extends App {
   final val ALPHA = 0.8
   final val ETA = 0.01
   final val GAMMA = 1.0
   final val SIZE_HIDDEN_LAYER = 3
   final val TEST_SIZE = 10
   final val NUM_EPOCHS = 100
   final val EPS = 1e-5
   
   def run(args: Array[String]): Unit =  {
  	 val x = Array.fill(TEST_SIZE)(Array[Double](0.2 + 0.4*Random.nextDouble, 0.2*Random.nextDouble))  
  	 val y = Array.tabulate(TEST_SIZE)(n => Array[Double](n/TEST_SIZE+ 0.1))  	 
  	 
  	 setNoSoftmax
  	 
  	 val config = MLPConfig(ALPHA, ETA, GAMMA, Array[Int](SIZE_HIDDEN_LAYER), NUM_EPOCHS, EPS)
  	 if( MLP[Double](config, x, y, new MLP.MLPRegression).model == None)
	     throw new IllegalStateException("Failed to train the model for alpha = " + ALPHA) 
   }
   
   run(null)
}


// ---------------------------------  EOF --------------------------------------------
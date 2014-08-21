/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap9

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.supervised.nnet.MLPConfig
import org.scalaml.supervised.nnet.MLP
import org.scalaml.core.XTSeries

import org.scalaml.supervised.nnet.MLPConnection.setNoSoftmax

/**
 *  @author Patrick Nicolas
 *  @since Jul 20, 2014
 *  @note Book
 */
object BinaryMLPEval {
   final val ALPHA = 0.85
   final val ETA = 0.005
   final val GAMMA = 1.0
   final val SIZE_HIDDEN_LAYER = 4
   final val NUM_EPOCHS = 100
   final val TEST_SIZE: Int  = 100
   final val EPS = 1e-4
   final val ACTIVATION = (x:Double) => Math.tanh(x)
   
   def run() {
  	  import scala.util.Random
  	  
  	  Console.println("Evaluation of Binary MLP evaluation")
  
  	  val HALF_TEST_SIZE = (TEST_SIZE>>1)
  	  def function1(x: Double): DblVector = Array[Double](0.1 + 0.2*Random.nextDouble, 0.1  +  0.2*Random.nextDouble)
  	  def function2(x: Double): DblVector = Array[Double](0.8  +  0.2*Random.nextDouble, 0.8  +  0.2*Random.nextDouble)
  	 
  	  val x = Array.tabulate(TEST_SIZE)(n => if( n < HALF_TEST_SIZE) function1(n) else function2(n))
      val y = Array.tabulate(TEST_SIZE)(n => if( n < HALF_TEST_SIZE) Array[Double](0.0) else Array[Double](1.0) )
      
  	  setNoSoftmax
      val config = MLPConfig(ALPHA, ETA, GAMMA,  SIZE_HIDDEN_LAYER, NUM_EPOCHS, EPS, ACTIVATION)
      val mlpClassifier = MLP[Double](config, x, y, new MLP.MLPClassification(y))
      
      val correct = x.zip(y).foldLeft(0)((s, xy) => {
	      val output = (mlpClassifier |> xy._1).get
	      if(output(0) - xy._2(0) < 0.25) s + 1 else s
       })
       println("Accuracy: " + correct.toDouble/x.size)

	      
      mlpClassifier |> Array[Double](0.9, 0.9) match {
      	case Some(output) => println(output(0))
      	case None => println("MLP classifier was not trained")
      }
      
   }
}

// --------------------------------- EOF -----------------------------
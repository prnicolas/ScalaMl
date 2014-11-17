/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 */
package org.scalaml.app.chap9

import org.scalaml.core.types.ScalaMl._
import org.scalaml.supervised.nnet.MLPConfig
import org.scalaml.supervised.nnet.MLP
import org.scalaml.core.XTSeries
import scala.util.{Try, Success, Failure}
import org.scalaml.util.Display
import org.apache.log4j.Logger
import org.scalaml.app.Eval

/**
 * <p>Singleton to evaluate the binary (2 class) multi-layer perceptron.</p>
 * 
 *  @author Patrick Nicolas
 *  @since July 20, 2014
 *  @note Scala for Machine Learning Chapter 9: Artificial Neural Network/Evaluation/Test case
 */
object BinaryMLPEval extends Eval {
   val name: String = "BinaryMLPEval"
  	 
   final val ALPHA = 0.85
   final val ETA = 0.01
   final val SIZE_HIDDEN_LAYER = Array[Int](4)
   final val NUM_EPOCHS = 4
   final val TEST_SIZE: Int  = 10
   final val EPS = 1e-3
   private val logger = Logger.getLogger(name)
   
   def run(args: Array[String]): Int = {
  	  import scala.util.Random
  	  
  	  Display.show(s"$name Evaluation of Binary MLP evaluation", logger)
        
  	  implicit val mlpObjective = new MLP.MLPBinClassifier
  	  val HALF_TEST_SIZE = (TEST_SIZE>>1)
  	  def function1(x: Double): DblVector = Array[Double](0.1 + 0.2*Random.nextDouble, 0.1  +  0.2*Random.nextDouble)
  	  def function2(x: Double): DblVector = Array[Double](0.8  + 0.2*Random.nextDouble, 0.8  +  0.2*Random.nextDouble)
  	 
  	  val x: Array[DblVector] = Array.tabulate(TEST_SIZE)(n => if( n < HALF_TEST_SIZE) function1(n) else function2(n))
      val y: Array[DblVector] = Array.tabulate(TEST_SIZE)(n => if( n < HALF_TEST_SIZE) Array[Double](0.0) else Array[Double](1.0) )
      
      val config = MLPConfig(ALPHA, ETA, SIZE_HIDDEN_LAYER, NUM_EPOCHS, EPS)
      val mlpClassifier = MLP[Double](config, x, y)
      
      val correct = x.zip(y).foldLeft(0)((s, xy) => {
	      val output = mlpClassifier |> xy._1
	      if(output(0) - xy._2(0) < 0.25) s + 1 else s
      })
      Display.show(s"$name Accuracy: ${correct.toDouble/x.size}", logger)
      val x0 = 0.9
      val y0 = 0.8
	  Try(mlpClassifier |> Array[Double](x0, y0))
	  match {
      	case Success(output) => Display.show(s"$name run for ($x0, $y0) is ${output(0)}. It should be 1", logger)
      	case Failure(e) => Display.error(s"$name run", logger, e)
      }     
   }
}



object BinaryMLPEvalApp extends App {
   BinaryMLPEval.run(Array.empty)
}

// --------------------------------- EOF -----------------------------
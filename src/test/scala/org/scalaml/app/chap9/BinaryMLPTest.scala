/**
 */
package org.scalaml.app.chap9

import org.scalaml.supervised.nnet.MLPConfig
import org.scalaml.supervised.nnet.MLP
import scala.util.Random
import org.scalaml.core.types.ScalaMl._
import org.apache.log4j.Logger
import org.scalaml.util.Display

/**
 *  @author Patrick Nicolas
 *  @date Sep 20, 2014
 *  @project Book
 */
object BinaryMLPTest extends App {
   private val logger = Logger.getLogger("BinaryMLPTest")
   final val ALPHA = 0.4
   final val ETA = 0.2
   final val SIZE_HIDDEN_LAYER = Array[Int](1)
   final val NUM_EPOCHS = 1000
   final val TEST_SIZE: Int  = 8
   final val EPS = 1e-4
   final val ACTIVATION = (x: Double) => 1.0/(1.0 + Math.exp(-x))

   val x1 = Array.tabulate(TEST_SIZE)(x => Array[Double](1.0 + 0.001*Random.nextDouble))
      
   val x2 =  Array.tabulate(TEST_SIZE)(x => Array[Double](2.0 + 0.001*Random.nextDouble))
   val z = Array.tabulate(TEST_SIZE)(x => Array[Double](0.3 + 0.05*Random.nextDouble))
   val x = x1 ++ x2
      
   val y: DblVector = Array.fill(TEST_SIZE)(0.0) ++ Array.fill(TEST_SIZE)(1.0)
   val config = MLPConfig(ALPHA, ETA, SIZE_HIDDEN_LAYER, NUM_EPOCHS, EPS, ACTIVATION)
       
   implicit val mlpObjective = new MLP.MLPBinClassifier
   val mlpClassifier = MLP[Double](config, x, y)
      
   val correct = x.zip(y).foldLeft(0)((s, xy) => {
	   val output = mlpClassifier |> xy._1
	   if(Math.abs(output(1) - xy._2) < 0.1) s + 1 else s
   })
   Display.show(s"Accuracy is ${correct.toDouble/x.size}, it should be 1.0", logger)
}

// -----------------------  EOF ----------------------------------------
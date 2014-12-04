/**
 */
package org.scalaml.app.chap9

import org.scalaml.supervised.nnet.MLPConfig
import org.scalaml.supervised.nnet.MLP
import scala.util.Random
import org.scalaml.core.Types.ScalaMl._
import org.apache.log4j.Logger
import org.scalaml.util.Display
import org.scalaml.app.Eval




object BinaryMLPTest extends Eval {
	val name: String = "BinaryMLPTest"
	val maxExecutionTime: Int = 25000
	
	private val logger = Logger.getLogger(name)

	private val ALPHA = 0.4
	private val ETA = 0.2
	private val SIZE_HIDDEN_LAYER = Array[Int](1)
	private val NUM_EPOCHS = 1000
	private val TEST_SIZE: Int  = 8
	private val EPS = 1e-4
	private val ACTIVATION = (x: Double) => 1.0/(1.0 + Math.exp(-x))

	private val x1 = Array.tabulate(TEST_SIZE)(x => Array[Double](1.0 + 0.001*Random.nextDouble))
	private val x2 =  Array.tabulate(TEST_SIZE)(x => Array[Double](2.0 + 0.001*Random.nextDouble))
	private val z = Array.tabulate(TEST_SIZE)(x => Array[Double](0.3 + 0.05*Random.nextDouble))
	private val x = x1 ++ x2
      
	private val y: DblVector = Array.fill(TEST_SIZE)(0.0) ++ Array.fill(TEST_SIZE)(1.0)
	
		/** <p>Execution of the scalatest for <b>MLP</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		val config = MLPConfig(ALPHA, ETA, SIZE_HIDDEN_LAYER, NUM_EPOCHS, EPS, ACTIVATION)
	       
		implicit val mlpObjective = new MLP.MLPBinClassifier
		val mlpClassifier = MLP[Double](config, x, y)
	      
		val correct = x.zip(y).foldLeft(0)((s, xy) => {
			val output = mlpClassifier |> xy._1
			if(Math.abs(output(1) - xy._2) < 0.1) 
				s + 1 
			else 
				s
		})
	
		Display.show(s"$name Accuracy is ${correct.toDouble/x.size}, it should be 1.0", logger)
	}
}

// -----------------------  EOF ----------------------------------------
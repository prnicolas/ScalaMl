/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.app.chap9

import org.scalaml.core.Types.ScalaMl
import org.scalaml.supervised.nnet.{MLPConfig, MLP}
import org.scalaml.core.XTSeries
import org.scalaml.util.{FormatUtils, DisplayUtils}
import org.scalaml.app.Eval

		/**
		 * <p><b>Purpose:</b>Singleton to evaluate the binary (2 class) multi-layer perceptron.</p>
		 * 
		 *  @author Patrick Nicolas
		 *  @note Scala for Machine Learning Chapter 9: Artificial Neural Network/Evaluation/Test case
		 */
object BinaryMLPEval extends Eval {
	import scala.util.{Try, Success, Failure}	
	import scala.reflect.ClassTag
	import org.apache.log4j.Logger
	import ScalaMl._
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "BinaryMLPEval"
	
	private val ALPHA = 0.85
	private val ETA = 0.01
	private val SIZE_HIDDEN_LAYER = Array[Int](4)
	private val NUM_EPOCHS = 4
	private val TEST_SIZE: Int  = 10
	private val EPS = 1e-3


		/** <p>Execution of the scalatest for <b>MLP</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		import scala.util.Random
  	  
		DisplayUtils.show(s"$header Binary Multi-layer perceptron", logger)
        
		implicit val mlpObjective = new MLP.MLPBinClassifier

		val HALF_TEST_SIZE = (TEST_SIZE>>1)
		def function1(x: Double): DblVector = 
				Array[Double](0.1 + 0.2*Random.nextDouble, 0.1  +  0.2*Random.nextDouble)
		def function2(x: Double): DblVector = 
				Array[Double](0.8  + 0.2*Random.nextDouble, 0.8  +  0.2*Random.nextDouble)
  	 
		val x: DblMatrix = Array.tabulate(TEST_SIZE)(n => 
				if( n < HALF_TEST_SIZE) function1(n) else function2(n))
		val y: DblMatrix = Array.tabulate(TEST_SIZE)(n => 
				if( n < HALF_TEST_SIZE) Array[Double](0.0) else Array[Double](1.0) )
      
		val config = MLPConfig(ALPHA, ETA, SIZE_HIDDEN_LAYER, NUM_EPOCHS, EPS)
		val mlpClassifier = MLP[Double](config, x, y)
      
		val correct = x.zip(y).foldLeft(0)((s, xy) => {
			val output = mlpClassifier |> xy._1
			if(output(0) - xy._2(0) < 0.25) s + 1 else s
		})

		val result = FormatUtils.format(correct.toDouble/x.size, "", FormatUtils.MediumFormat)
		DisplayUtils.show(s"$name Accuracy: $result", logger)
		
		val x0 = 0.9
		val y0 = 0.8
		Try(mlpClassifier |> Array[Double](x0, y0))
		match {
			case Success(output) => 
					DisplayUtils.show(s"$name.run for ($x0, $y0) is ${output(0)}. It should be 1", logger)
			case Failure(e) => failureHandler(e)
		}     
	}
}


// --------------------------------- EOF -----------------------------
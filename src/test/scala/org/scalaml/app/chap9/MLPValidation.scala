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

import org.scalaml.supervised.nnet.{MLPConfig, MLP}
import org.scalaml.core.XTSeries
import org.scalaml.util.DisplayUtils
import org.scalaml.app.Eval


		/**
		 * <p><b>Purpose:</b>Singleton to va;odate the Multi-layer perceptron using
		 * randomly generated synthetic data.</p>
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning Chapter 9: Artificial Neural Network / Evaluation
		 */
object MLPValidation extends Eval {
	import scala.util.{Random, Try, Success, Failure}
	import org.apache.log4j.Logger
	import MLP._
		/**
		 * Name of the evaluation class
		 */
	val name: String = "MLPValidation"
   
	private val ALPHA = 0.8
	private val ETA = 0.01
	private val SIZE_HIDDEN_LAYER = 5
	private val TEST_SIZE = 10
	private val NUM_EPOCHS = 100
	private val EPS = 1e-5

		/** 
		 * <p>Execution of the scalatest for <b>MLP</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(arg: Array[String]) : Int =  {
		DisplayUtils.show(s"$header Validation MLP with synthetic data", logger)
        
		val x = Array.fill(TEST_SIZE)(Array[Double](0.2 + 0.4*Random.nextDouble, 0.2*Random.nextDouble))  
		val y = Array.tabulate(TEST_SIZE)(n => Array[Double](n/TEST_SIZE+ 0.1))  	 
		
		Try {
			val state = MLPConfig(ALPHA, ETA, Array[Int](SIZE_HIDDEN_LAYER), NUM_EPOCHS, EPS)
	  	  
			implicit val mlpObjective = new MLP.MLPBinClassifier
			DisplayUtils.show(s"$name Creation of MLP model for a binary classifier", logger)
			MLP[Double](state, x, y).model.map(m => DisplayUtils.show(m.toString, logger)
			)
			.getOrElse(DisplayUtils.error(s"$name Incomplete model for alpha:$ALPHA, eta:$ETA", logger))
		}
		match {
		  case Success(n) => n
		  case Failure(e) => failureHandler(e)
		}
	}
}

// ---------------------------------  EOF --------------------------------------------
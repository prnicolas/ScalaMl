/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.app.chap9


import org.scalaml.supervised.nnet._
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import scala.util.Random
import MLP._
import org.apache.log4j.Logger
import org.scalaml.util.Display
import org.scalaml.app.Eval



object MLPValidation extends Eval {
	val name: String = "MLPValidation"
    val maxExecutionTime: Int = 25000
	private val logger = Logger.getLogger(name)
   
	private val ALPHA = 0.8
	private val ETA = 0.01
	private val SIZE_HIDDEN_LAYER = 5
	private val TEST_SIZE = 10
	private val NUM_EPOCHS = 100
	private val EPS = 1e-5

		/** <p>Execution of the scalatest for <b>MLP</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(arg: Array[String]) : Int =  {
		Display.show(s"\n\n *****  test#${Eval.testCount} $name Validation MLP with synthetic data", logger)
        
		val x = Array.fill(TEST_SIZE)(Array[Double](0.2 + 0.4*Random.nextDouble, 0.2*Random.nextDouble))  
		val y = Array.tabulate(TEST_SIZE)(n => Array[Double](n/TEST_SIZE+ 0.1))  	 
  	 
		val state = MLPConfig(ALPHA, ETA, Array[Int](SIZE_HIDDEN_LAYER), NUM_EPOCHS, EPS)
  	  
		implicit val mlpObjective = new MLP.MLPBinClassifier
		if( MLP[Double](state, x, y).model == None ) 
			Display.error(s"$name Failed to train the model for alpha = $ALPHA, eta = $ETA", logger)
		else
			Display.show(s"$name Creation of MLP model succeeds", logger)
	}
}



// ---------------------------------  EOF --------------------------------------------
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

import org.scalaml.app.ScalaMlTest


		/**
		 * <p>Test driver for the techniques described in the Chapter 9 Artificial neural networks<br>
		 * <ul>
		 * 	 <li>Multi-layer perceptron - eta factor</li>
		 *   <li>Multi-layer perceptron - alpha factor</li>
		 *   <li>Multi-layer perceptron - validation</li>
		 *   <li>Multi-layer perceptron - Synthetic binomial classification</li>
		 *   <li>Multi-layer perceptron - binomial classification for ETFs</li>
		 * </ul></p>
		 * @see org.scalaml.app.ScalaMlTest
		 * @author Patrick Nicolas
		 * @since May 28, 2014
		 * @note Scala for Machine Learning Chapter 9 Artificial neural networks
		 */
final class Chap9 extends ScalaMlTest {
		/**
		 * Name of the chapter the tests are related to
		 */
	val chapter: String = "Chapter 9"
		/**
		 * Maximum duration allowed for the execution of the evaluation
		 */
	val maxExecutionTime: Int = 40
	
	test(s"$chapter Multi-layer perceptron - eta factor") {
		evaluate(MLPConfigEval, Array[String]("eta"))
	}
  
	test(s"$chapter Multi-layer perceptron - alpha factor") {
		evaluate(MLPConfigEval, Array[String]("alpha"))
	}
  
	test(s"$chapter Multi-layer perceptron - validation") {
		evaluate(MLPValidation)
	}
	test(s"$chapter Multi-layer perceptron - Synthetic binomial classification") {
		evaluate(BinaryMLPEval)
	}
  
	test(s"$chapter Multi-layer perceptron - binomial classification for ETFs") {
		evaluate(MLPEval)
	}
}


// ---------------------------   EOF ------------------------------------------------
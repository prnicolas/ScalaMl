/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99
 */
package org.scalaml.app.chap9

import org.scalaml.app.ScalaMlTest


		/**
		 * Test driver for the techniques described in the Chapter 9 '''Artificial neural networks'''
		 * {{{
		 * 	 Multi-layer perceptron - eta factor
		 *   Multi-layer perceptron - alpha factor
		 *   Multi-layer perceptron - validation
		 *   Multi-layer perceptron - Synthetic binomial classification
		 *   Multi-layer perceptron - binomial classification for Exchange Traded Funds.
		 * }}}
		 * @see org.scalaml.app.ScalaMlTest
		 * @author Patrick Nicolas
		 * @since 0.98 May 28, 2014
		 * @see Scala for Machine Learning Chapter 9 Artificial neural networks
		 */
final class Chap9 extends ScalaMlTest {
		/**
		 * Name of the chapter the tests are related to
		 */
	val chapter: String = "Chapter 9"

		/**
		 * Maximum duration allowed for the execution of the evaluation
		 */
	val maxExecutionTime: Int = -1
	
	test(s"$chapter Multi-layer perceptron - eta factor") {
		evaluate(MLPConfigEval, Array[String]("eta"))
	}
  
	test(s"$chapter Multi-layer perceptron - alpha factor") {
		evaluate(MLPConfigEval, Array[String]("alpha"))
	}
  
	test(s"$chapter Multi-layer perceptron - Single hidden layer binomial classification") {
		evaluate(BinaryMLPEval)
	}
	
	test(s"$chapter Multi-layer perceptron - Two hidden layers 4x4 binomial classification") {
		evaluate(BinaryDeepMLPEval, Array[String]("4", "4"))
	}
  
	test(s"$chapter Multi-layer perceptron - Two hidden layers 7x7 binomial classification") {
		evaluate(BinaryDeepMLPEval, Array[String]("7", "7"))
	}
	
	test(s"$chapter Multi-layer perceptron - Three hidden layers 5x6x5 binomial classification") {
		evaluate(BinaryDeepMLPEval, Array[String]("5", "6", "5"))
	}
	
	test(s"$chapter Multi-layer perceptron - binomial classification for ETFs") {
		evaluate(MLPEval)
	}
}

// ---------------------------   EOF ------------------------------------------------
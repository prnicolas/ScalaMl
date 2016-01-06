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
package org.scalaml.app.chap1

import org.scalaml.app.ScalaMlTest


		/**
		 * Test driver for the techniques described in the Chapter 1 "Getting started"
		 * 
		 * - Logistic Binary classifier
		 * - JFreeChart plots
		 * @see org.scalaml.app.ScalaMlTest
		 * @author Patrick Nicolas
		 * @since 0.98.1 May 28, 2014
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 1 Getting Started
		 */
final class Chap1 extends ScalaMlTest {
		/**
		 * Name of the chapter the tests are related to
		 */
	val chapter: String = "Chapter 1"
	
	val maxExecutionTime: Int = 5
	
	test(s"$chapter Simple binomial logistic regression") {
		evaluate(LogBinRegressionEval)
	}
	
	test(s"$chapter Simple binomial logistic regression validation") {
		evaluate(LogBinRegressionEval2)
	}
	
	test(s"$chapter JFreeChart Plots") {
		evaluate(PlotterEval)
	}
	
	test(s"$chapter Min-Max") {
		evaluate(MinMaxEval)
	}
}

// --------------------  EOF --------------------------------------
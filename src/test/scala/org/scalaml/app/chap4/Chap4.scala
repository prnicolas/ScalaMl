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
package org.scalaml.app.chap4

import org.scalatest.FunSuite
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.DataSource
import org.scalaml.app.{ScalaMlTest, Eval}


		/**
		 * Trait to evaluate the filtering techniques presented in Chapter 4
		 * @see org.scalaml.app.Eval
		 * @author Patrick Nicolas
		 * @since 0.98 May 28, 2014
		 * @see Scala for Machine Learning Chapter 3 "Data pre-processing"
		 */
trait UnsupervisedLearningEval extends Eval {
	final val path = "resources/data/chap4/"

		/**
		 * Execution of the scalatest for Unsupervised techniques.
		 * @param args array of arguments used in the test
		 */
	override protected def run(args: Array[String] = Array.empty[String]): Int
	
	protected val extractor = YahooFinancials.adjClose :: List[Array[String] =>Double]()
	protected def symbolFiles = DataSource.listSymbolFiles(path)
}

		/**
		 * Test driver for the techniques described in the Chapter 4 Unsupervised learning
		 * {{{
		 *   K-means clustering
		 *   Expectation-Maximization clustering
		 *   Principal Components Analysis
		 * }}}
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98.1 May 28, 2014
		 * @see Scala for Machine Learning Chapter 4 "Unsupervised learning"
		 * @see org.scalaml.app.ScalaMlTest
		 */
final class Chap4 extends ScalaMlTest { 
		/**
		 * Name of the chapter the tests are related to
		 */
	val chapter: String = "Chap 4"
		/**
		 * Maximum duration allowed for the execution of the evaluation
		 */
	val maxExecutionTime: Int = 12
    
	test(s"$chapter K-means clustering") {
		val input = Array[String]("2", "3", "4", "7", "9", "10", "13", "15")
		evaluate(KMeansEval, input)
	}
	
	test(s"$chapter K-means clustering validation") {
		evaluate(KMeansEval2)
	}
	
	test(s"$chapter Expectation-Maximization clustering") {
		evaluate(EMEval, Array[String]("2", "40"))
		evaluate(EMEval, Array[String]("3", "25"))
		evaluate(EMEval, Array[String]("4", "15"))
	}
	
	test(s"$chapter Principal Components Analysis") {
		evaluate(PCAEval)
	}
}


// -----------------------------------  EOF ---------------------------------------------------
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
package org.scalaml.app.chap4


import org.scalatest.FunSuite
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.DataSource
import org.scalaml.app.{ScalaMlTest, Eval}


		/**
		 * Trait to evaluate the filtering techniques presented in Chapter 4
		 * @see org.scalaml.app.Eval
		 * @author Patrick Nicolas
		 * @since May 28, 2014
		 * @note Scala for Machine Learning Chapter 3 Data pre-processing
		 */
trait UnsupervisedLearningEval extends Eval {
	final val path = "resources/data/chap4/"

		/**
		 * <p>Execution of the scalatest for Unsupervised techniques.</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String] = Array.empty): Int
	
	protected val extractor = YahooFinancials.adjClose :: List[Array[String] =>Double]()
	protected def symbolFiles = DataSource.listSymbolFiles(path)
}

		/**
		 * <p>Test driver for the techniques described in the Chapter 4 Unsupervised learning<br>
		 * <ul>
		 * 	 <li>K-means clustering</li>
		 *   <li>Expectation-Maximization clustering</li>
		 *   <li>Principal Components Analysis</li>
		 * </ul></p>
		 * @see org.scalaml.app.ScalaMlTest
		 * @author Patrick Nicolas
		 * @since May 28, 2014
		 * @note Scala for Machine Learning Chapter 4 Unsupervised learning
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
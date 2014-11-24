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
package org.scalaml.app.chap4


import org.scalatest.FunSuite
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.DataSource
import org.scalaml.app.ScalaMlTest
import org.scalaml.app.Eval

trait UnsupervisedLearningEval extends Eval {
	final val path = "resources/data/chap4/"
       
	def run(args: Array[String] = Array.empty): Int
	protected val extractor = YahooFinancials.adjClose :: List[Array[String] =>Double]()
	protected def symbolFiles = DataSource.listSymbolFiles(path)
}


final class Chap4 extends ScalaMlTest { 
	val chapter: String = "Chap 3"
    
	test("K-means evaluation") {
		val input = Array[String]("2", "3", "4", "7", "9", "10", "13", "15")
		evaluate(KMeansEval, input)
	}
	
	test("Expectation-Maximization evaluation") {
		evaluate(EMEval, Array[String]("2", "40"))
		evaluate(EMEval, Array[String]("3", "25"))
		evaluate(EMEval, Array[String]("4", "15"))
	}
	
	test("Principal Components Evaluation") {
		evaluate(PCAEval)
	}
}


// -----------------------------------  EOF ---------------------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95
 */
package org.scalaml.app.chap6

import org.scalatest.FunSuite



final class Chap6 extends FunSuite {
	test("Single linear regression evaluation") {
	   assert(SingleLinearRegressionEval.run >= 0, "Chapter 6 SingleLinearRegressionEval failed")
	}

	test("Least squares regression trending evaluation") {
		 assert(MultiLinearRegressionEval.run(Array[String]("trend")) >= 0, "Chapter 6 MultiLinearRegressionEval for trends failed")
	}
	
	test("Least squares regression features selection evaluation") {
		assert(MultiLinearRegressionEval.run(Array[String]("filter")) >= 0, "Chapter 6 MultiLinearRegressionEval for features selection failed")
	}
	
	test("Ridge regression evaluation") {
		assert(RidgeRegressionEval.run >= 0, "Chapter 6 Ridge regression failed")
	}
	
	test("Binomial logistic regression evaluation") {
		assert(LogisticRegressionEval.run >= 0, "Chapter 6 Binomial Logistic regression failed")
	}
}


// ----------------------------  EOF ----------------------------------
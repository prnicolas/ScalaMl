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
package org.scalaml.app.chap6

import org.scalaml.app.ScalaMlTest




final class Chap6 extends ScalaMlTest {
	val chapter: String = "Chap 6"
	val maxExecutionTime: Int = 7000
	
	test(s"$chapter Single linear regression evaluation") {
		evaluate(SingleLinearRegressionEval)
	}

	test(s"$chapter Least squares regression trending evaluation") {
		evaluate(MultiLinearRegressionEval, Array[String]("trending"))
	}
	
	test(s"$chapter Least squares regression features selection evaluation") {
		evaluate(MultiLinearRegressionEval, Array[String]("features"))
	}
	
	test(s"$chapter Ridge regression evaluation") {
		evaluate(RidgeRegressionEval)
	}
	
	test(s"$chapter Binomial logistic regression evaluation") {
		evaluate(LogisticRegressionEval)
	}
}


// ----------------------------  EOF ----------------------------------
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
package org.scalaml.app.chap6

import org.scalaml.app.ScalaMlTest


		/**
		 * Test driver for the techniques described in the Chapter 6 Regression and Regularization
		 * <ul>
		 * 	 <li>Single variable linear regression</li>
		 *   <li>Least squares regression for trend analysis</li>
		 *   <li>Least squares regression for features selection</li>
		 *   <li>Ridge regression</li>
		 *   <li>Binomial logistic regression</li>
		 * </ul>
		 * @see org.scalaml.app.ScalaMlTest
		 * @author Patrick Nicolas
		 * @since May 28, 2014
		 * @note Scala for Machine Learning Chapter 6 Regression and Regularization
		 */
final class Chap6 extends ScalaMlTest {
		/**
		 * Name of the chapter the tests are related to
		 */
	val chapter: String = "Chap 6"
	
	test(s"$chapter Single variable linear regression") {
		evaluate(SingleLinearRegressionEval)
	}

	test(s"$chapter Least squares regression for trend analysis") {
		evaluate(MultiLinearRegrTrendsEval)
	}
	
	test(s"$chapter Least squares regression for features selection") {
		evaluate(MultiLinearRegrFeaturesEval)
	}
	
	test(s"$chapter Ridge regression") {
		evaluate(RidgeRegressionEval)
	}
	
	test(s"$chapter Binomial logistic regression") {
		evaluate(LogisticRegressionEval)
	}
}


// ----------------------------  EOF ----------------------------------
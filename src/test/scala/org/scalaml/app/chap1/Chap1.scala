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
package org.scalaml.app.chap1

import org.scalaml.app.ScalaMlTest



		/**
		 * <p>Test driver for the techniques described in this chapter<br>
		 * <ul>
		 *   <li>JFreeChart plots</li>
		 *   <li>Logistic Binary classifier</li>
		 * </ul></p>
		 * @author Patrick Nicolas
		 * @since December 11, 2013
		 * @note Scala for Machine Learning.
		 */
final class Chap1 extends ScalaMlTest {
	val chapter: String = "Chap 1"
	val maxExecutionTime: Int = 5000
	
	test(s"$chapter Simple Binary Logistic Regression") {
		evaluate(LogBinRegressionEval)
	}
	
	test(s"$chapter JFreeChart Plotting") {
		evaluate(PlotterEval)
	}
}

// --------------------  EOF --------------------------------------
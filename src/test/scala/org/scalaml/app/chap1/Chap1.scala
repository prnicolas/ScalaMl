/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.94
 */
package org.scalaml.app.chap1

import org.scalatest.FunSuite



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
final class Chap1 extends FunSuite {
	test("Simple Binary Logistic Regression") {
	   assert(LogBinRegressionEval.run == 0)
	}
	
	test("JFreeChart Plotting") {
		assert(PlotterEval.run == 0)
	}
}

// --------------------  EOF --------------------------------------
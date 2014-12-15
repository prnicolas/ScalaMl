/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.3
 */
package org.scalaml.app.chap2


import org.scalaml.app.ScalaMlTest



		/**
		 * <p>Test driver for the techniques described in the chapter 2 "Hello World!"<br>
		 * <ul>
		 *   <li>JDependency injection base workflow</li>
		 *   <li>Bias-Variance decomposition</li>
		 * </ul></p>
		 * @see org.scalaml.app.ScalaMlTest
		 * @author Patrick Nicolas
		 * @since May 28, 2014
		 * @note Scala for Machine Learning chapter 2 "Hello World!"
		 */
final class Chap2 extends ScalaMlTest {
		/**
		 * Name of the chapter the tests are related to
		 */
	val chapter: String = "Chapter 2"
   
	test(s"$chapter Workflow evaluation") {
		evaluate(WorkflowEval)
	}
   
	test(s"$chapter Variance - Bias decomposition evaluation") {
		evaluate(BiasVarianceEval)
	}
}

     
// -------------------------------------  EOF -----------------------------------------
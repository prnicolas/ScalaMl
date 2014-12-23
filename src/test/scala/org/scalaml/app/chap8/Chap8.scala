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
package org.scalaml.app.chap8

import org.scalaml.app.ScalaMlTest


		/**
		 * <p>Test driver for the techniques described in the Chapter 8 Kernel models and support 
		 * vector machines<br>
		 * <ul>
		 * 	 <li>Support Vector machines margin</li>
		 *   <li>Support Vector machines kernel function</li>
		 *   <li>Support Vector classifier</li>
		 *   <li>One-class Support Vector classifier</li>
		 *   <li>Support Vector regression</li>
		 * </ul></p>
		 * @see org.scalaml.app.ScalaMlTest
		 * @author Patrick Nicolas
		 * @since May 28, 2014
		 * @note Scala for Machine Learning Chapter 8 Kernel models and support vector machines
		 */
final class Chap8 extends ScalaMlTest {
		/**
		 * Name of the chapter the tests are related to
		 */
	val chapter = "Chap 8"
		/**
		 * Maximum duration allowed for the execution of the evaluation
		 */
	val maxExecutionTime: Int = 35
	test(s"$chapter Support Vector machines margin") {
		evaluate(SVCMarginEval)
	}
   
	test(s"$chapter Support Vector machines kernel function") { 
		evaluate(SVCKernelEval)
	}
    
	test(s"$chapter Support Vector classifier") {
		evaluate(SVCEval)
	}
    
	test(s"$chapter One-class Support Vector classifier") {
		evaluate(SVCOutliersEval)
	}
	
	test(s"$chapter Support Vector regression") {
		evaluate(SVREval)
	}
}



// --------------------------- EOF --------------------------------------------------
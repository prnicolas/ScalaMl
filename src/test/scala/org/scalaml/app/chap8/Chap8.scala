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
package org.scalaml.app.chap8

import org.scalaml.app.ScalaMlTest


		/**
		 * Test driver for the techniques described in the Chapter 8 Kernel models and support 
		 * vector machines
		 * 
		 * 	- Support Vector machines margin
		 *  
		 *  - Support Vector machines kernel function
		 *  
		 *  - Support Vector classifier
		 *  
		 *  - One-class Support Vector classifier
		 *  
		 *  - Support Vector regression
		 * @see org.scalaml.app.ScalaMlTest
		 * @author Patrick Nicolas
		 * @since May 28, 2014
		 * @see Scala for Machine Learning Chapter 8 ''Kernel models and support vector machines''
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
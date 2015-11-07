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
package org.scalaml.app.chap2


import org.scalaml.app.ScalaMlTest



		/**
		 * Test driver for the techniques described in the chapter 2 "Hello World!"
		 * 
		 * - Dependency injection base workflow
		 * - Bias-Variance decomposition
		 * @see org.scalaml.app.ScalaMlTest
		 * @author Patrick Nicolas
		 * @since 0.98.1 May 28, 2014
		 * @version 0.98.2
		 * @see Scala for Machine Learning chapter 2 "Hello World!"
		 */
final class Chap2 extends ScalaMlTest {
		/**
		 * Name of the chapter the tests are related to
		 */
	val chapter: String = "Chapter 2"
		/**
		 * Maximum duration allowed for the execution of the evaluation
		 */
	val maxExecutionTime: Int = 6
	test(s"$chapter Workflow evaluation") {
		evaluate(WorkflowEval)
	}
   
	test(s"$chapter Variance - Bias decomposition evaluation") {
		evaluate(BiasVarianceEval)
	}
	
	test(s"$chapter Matrix manipulation") {
		evaluate(DMatrixEval)
	}
	
	test(s"$chapter Basic statistics") {
		evaluate(StatsEval)
	}
	
	test(s"$chapter transform using an explicit configuration") {
		evaluate(ETransformEval)
	}
	
	test(s"$chapter transform using model implicitly derived from input data") {
		evaluate(ITransformEval)
	}
	
	test(s"$chapter One fold cross-validation") {
		evaluate(OneFoldXValidationEval)
	}
}


     
// -------------------------------------  EOF -----------------------------------------
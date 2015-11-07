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
package org.scalaml.app.chap7

import org.scalaml.app.ScalaMlTest



		/**
		 * Test driver for the techniques described in the Chapter 7 Sequential data models
		 * 
		 * - Hidden Markov Model evaluation form
		 * 
		 * - Hidden Markov Model decoding /Viterbi form
		 *  
		 * - Hidden Markov Model training
		 * 
		 * - Conditional Random Fields
		 * 
		 * @see org.scalaml.app.ScalaMlTest
		 * @author Patrick Nicolas
		 * @since 0.98.2 May 28, 2014
		 * @see Scala for Machine Learning Chapter 7 ''Sequential data model''
		 */
final class Chap7 extends ScalaMlTest {
		/**
		 * Name of the chapter the tests are related to
		 */
	val chapter: String = "Chap 7"
	
	  	/**
		 * Maximum duration allowed for the execution of the evaluation
		 */
	val maxExecutionTime: Int = 25
	
	test(s"$chapter Hidden Markov Model evaluation form with 2 state values") {
		evaluate(HMMEvaluationEval)
	}
   
	test(s"$chapter Hidden Markov Model training form") {
		evaluate(HMMTrainingEval)
	}
	
	test(s"$chapter Hidden Markov Model decoding form") {
		evaluate(HMMDecodingEval)
	}
	
	test(s"$chapter Hidden Markov Model decoding form 2") {
		evaluate(HMMDecodingEval2)
	}

	test(s"$chapter Conditional Random Fields") {
		evaluate(CrfEval)
	}
}

// --------------------------------  EOF -------------------------------
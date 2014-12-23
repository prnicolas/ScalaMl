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
package org.scalaml.app.chap5

import org.scalaml.app.ScalaMlTest



		/**
		 * <p>Test driver for the techniques described in the Chapter 5 Naive Bayes models<br>
		 * <ul>
		 * 	 <li>Binomial Naive Bayes</li>
		 *   <li>Naive Bayes text analysis</li>
		 *   <li>Naive Bayes and function classification</li>
		 * </ul></p>
		 * @see org.scalaml.app.ScalaMlTest
		 * @author Patrick Nicolas
		 * @since May 28, 2014
		 * @note Scala for Machine Learning Chapter 5 Naive Bayes models
		 */
final class Chap5 extends ScalaMlTest {
		/**
		 * Name of the chapter the tests are related to
		 */
	val chapter: String = "Chap5"
		/**
		 * Maximum duration allowed for the execution of the evaluation
		 */
	val maxExecutionTime: Int = 10
	
	test(s"$chapter Binomial Naive Bayes") {
		val TRAIN_VALIDATION_RATIO = "0.8"
		evaluate(BinomialBayesEval, Array[String]("IBM", TRAIN_VALIDATION_RATIO, "8"))
  	  
		evaluate(BinomialBayesEval, Array[String]("NEM", TRAIN_VALIDATION_RATIO, "4"))
		evaluate(BinomialBayesEval, Array[String]("NEM", TRAIN_VALIDATION_RATIO, "12"))
		evaluate(BinomialBayesEval, Array[String]("NEM", TRAIN_VALIDATION_RATIO, "36"))
	  
		evaluate(BinomialBayesEval, Array[String]("GE", TRAIN_VALIDATION_RATIO, "4"))
		evaluate(BinomialBayesEval, Array[String]("GE", TRAIN_VALIDATION_RATIO, "12"))
		evaluate(BinomialBayesEval, Array[String]("GE", TRAIN_VALIDATION_RATIO, "36"))
	  
		evaluate(BinomialBayesEval, Array[String]("BAC", TRAIN_VALIDATION_RATIO, "4"))
		evaluate(BinomialBayesEval, Array[String]("BAC", TRAIN_VALIDATION_RATIO, "12"))
		evaluate(BinomialBayesEval, Array[String]("BAC", TRAIN_VALIDATION_RATIO, "36"))
	}
   
	test(s"$chapter Naive Bayes text analysis") {
		evaluate(TextBayesEval)
	}
   
	test(s"$chapter Naive Bayes and function classification") {
		evaluate(FunctionClassificationEval)
	}
}


// -----------------------------  EOF ------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95
 */
package org.scalaml.app.chap5

import org.scalatest.FunSuite


final class Chap5 extends FunSuite {
   test("Chapter 5: Binomial Naive Bayes evaluation") {
  	  assert(BinomialBayesEval.run(Array[String]("0.5", "8")) >=0, "Chap5.BinomialBayesEval failed")
  	  
  	  val TRAIN_VALIDATION_RATIO = "0.8"
	  assert(BinomialBayesEval.run(Array[String]("NEM", TRAIN_VALIDATION_RATIO, "4")) >=0, "Chap5.BinomialBayesEval failed")
	  assert(BinomialBayesEval.run(Array[String]("NEM", TRAIN_VALIDATION_RATIO, "12")) >=0, "Chap5.BinomialBayesEval failed")
	  assert(BinomialBayesEval.run(Array[String]("NEM", TRAIN_VALIDATION_RATIO, "36")) >=0, "Chap5.BinomialBayesEval failed")
	  
	  assert(BinomialBayesEval.run(Array[String]("GE", TRAIN_VALIDATION_RATIO, "4")) >=0, "Chap5.BinomialBayesEval failed")
	  assert(BinomialBayesEval.run(Array[String]("GE", TRAIN_VALIDATION_RATIO, "12")) >=0, "Chap5.BinomialBayesEval failed")
	  assert(BinomialBayesEval.run(Array[String]("GE", TRAIN_VALIDATION_RATIO, "36")) >=0, "Chap5.BinomialBayesEval failed")
	  
	  assert(BinomialBayesEval.run(Array[String]("BAC", TRAIN_VALIDATION_RATIO, "4")) >=0, "Chap5.BinomialBayesEval failed")
	  assert(BinomialBayesEval.run(Array[String]("BAC", TRAIN_VALIDATION_RATIO, "12")) >=0, "Chap5.BinomialBayesEval failed")
	  assert(BinomialBayesEval.run(Array[String]("BAC", TRAIN_VALIDATION_RATIO, "36")) >=0, "Chap5.BinomialBayesEval failed")
   }
   
   test("Chapter 5: Naive Bayes text analysis evaluation") {
  	  assert(TextBayesEval.run >= 0, "Chap5. Text Analysis using Naive Bayes failed")
   }
   
   test("Chapte 5: Naive Bayes and function classification") {
  	   	assert(FunctionClassificationEval.run >= 0, "Chap5. Text Analysis using Naive Bayes failed")
   }
}


// -----------------------------  EOF ------------------------------------
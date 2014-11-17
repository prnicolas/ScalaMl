/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 */
package org.scalaml.app.chap9

import org.scalaml.app.ScalaMlTest


final class Chap9 extends ScalaMlTest {
  val chapter: String = "Chap 9"
  	
  test(s"$chapter Multi-perceptron eta factor evaluation") {
	  evaluate(MLPConfigEval, Array[String]("eta"))
  }
  
  test(s"$chapter Multi-perceptron alpha factor evaluation") {
	  evaluate(MLPConfigEval, Array[String]("alpha"))
  }
  
  test(s"$chapter Multi-perceptron validation") {
	  evaluate(MLPValidation)
  }
  
  test(s"$chapter Multi-perceptron binomial classification evaluation") {
	 evaluate(BinaryMLPEval)
  }
  
  test(s"$chapter Multi-perceptron binomial classification evaluation") {
	 evaluate(MLPEval)
  }
}


// ---------------------------   EOF ------------------------------------------------
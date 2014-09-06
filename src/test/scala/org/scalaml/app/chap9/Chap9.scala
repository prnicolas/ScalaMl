/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.app.chap9

import org.scalatest.FunSuite



final class Chap9 extends FunSuite {
  test("Multi-perceptron eta factor evaluation") {
	 MLPConfigEval.run(Array[String]("eta"))
  }
  
  test("Multi-perceptron alpha factor evaluation") {
	 MLPConfigEval.run(Array[String]("alpha")) 
  }
  
  test("Multi-perceptron validation") {
	 MLPValidation.run(null) 
  }
  
  test("Multi-perceptron binomial classification evaluation") {
	 BinaryMLPEval.run
  }
}


// ---------------------------   EOF ------------------------------------------------
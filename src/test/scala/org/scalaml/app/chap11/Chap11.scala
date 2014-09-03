/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.app.chap11

import org.scalaml.app.ScalaMlApp
import org.scalatest.FunSuite



final class Chap11 extends FunSuite {  
   test("QLearning evaluation")  {
  	  QLearningEval.run
   }
}

// ------------------------------------  EOF ----------------------------------
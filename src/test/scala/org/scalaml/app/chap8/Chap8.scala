/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95d
 */
package org.scalaml.app.chap8

import org.scalaml.app.ScalaMlTest


	/**
	 * <p>Class to run the test cases presented in chapter 8
	 * 
	 *  @author Patrick Nicolas
	 *  @since May 7. 2014
	 *  @note Scala for Machine Learning
	 */
final class Chap8 extends ScalaMlTest {
   val chapter = "Chap 8"
  	 
   test(s"$chapter Support Vector machines margin factor evaluation") {
  	  evaluate(SVCMarginEval)
   }
   
    test(s"$chapter  Support Vector machines kernel function evaluation") { 
      evaluate(SVCKernelEval)
    }
    
    test(s"$chapter Support Vector classifier evaluation") {
	   evaluate(SVCEval)
    }
    
    test(s"$chapter One-class Support Vector classifier evaluation") {
	   evaluate(SVCOutliersEval)
    }
}


object Chap8App extends App {
	val chap = new Chap8
	chap.evaluate(SVCMarginEval)
}


// --------------------------- EOF --------------------------------------------------
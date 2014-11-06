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
package org.scalaml.app.chap8

import org.scalatest.FunSuite



	/**
	 * <p>Class to run the test cases presented in chapter 8
	 * 
	 *  @author Patrick Nicolas
	 *  @since May 7. 2014
	 *  @note Scala for Machine Learning
	 */
final class Chap8 extends FunSuite {

   test("Support Vector machines margin factor evaluation") {
	 SVCMarginEval.run
   }
   
    test("Support Vector machines kernel function evaluation") {
	  SVCKernelEval.run
    }
    
    test("Support Vector classifier evaluation") {
	 SVCEval.run
    }
    
    test("One-class Support Vector classifier evaluation") {
	   SVCOutliersEval.run
    }
}


// --------------------------- EOF --------------------------------------------------
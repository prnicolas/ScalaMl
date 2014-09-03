/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.app.chap2


import org.scalatest.FunSuite



		/**
		 * <p>Test driver for the techniques described in this chapter<br>
		 * <ul>
		 *   <li>JDependency injection base workflow</li>
		 *   <li>Bias-Variance decomposition</li>
		 * </ul></p>
		 * 
		 * @author Patrick Nicolas
		 * @since February 1, 201t
		 * @note Scala for Machine Learning.
		 */
final class Chap2 extends FunSuite {
   test("Workflow evaluation") {
  	 WorkflowEval
   }
   
   test("Variance - Bias decomposition evaluation") {
  	  	 BiasVarianceEval.run
   }
}

     
// -------------------------------------  EOF -----------------------------------------
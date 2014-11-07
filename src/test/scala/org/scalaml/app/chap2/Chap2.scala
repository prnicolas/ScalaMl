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
package org.scalaml.app.chap2


import org.scalaml.app.ScalaMlTest



		/**
		 * <p>Test driver for the techniques described in this chapter<br>
		 * <ul>
		 *   <li>JDependency injection base workflow</li>
		 *   <li>Bias-Variance decomposition</li>
		 * </ul></p>
		 * 
		 * @author Patrick Nicolas
		 * @since February 1, 20
		 * @note Scala for Machine Learning.
		 */
final class Chap2 extends ScalaMlTest {
   val chapter: String = "Chap 2"
  	 
   test(s"$chapter Workflow evaluation") {
  	  evaluate(WorkflowEval)
   }
   
   test(s"$chapter Variance - Bias decomposition evaluation") {
  	  evaluate(BiasVarianceEval)
   }
}

     
// -------------------------------------  EOF -----------------------------------------
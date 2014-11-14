/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.app

import org.scalatest.FunSuite

		/**
		 * <p>Generic template for the scalatest invocation.</p>
		 * @author Patrick Nicolas
		 * @since July, 13, 2014
		 * @note Scala for Machine Learning
		 */
trait ScalaMlTest extends FunSuite {
	val chapter: String
  
		/**
		 * <p>Trigger the execution of a Scala test for a specific method and set of arguments.</p>
		 * @param args argument for the Scala test
		 * @param method Name of the method to be tested.
		 */
  def evaluate(eval: Eval, args: Array[String] = Array.empty): Unit = 
  	 assert(eval.run(args) >= 0, s"$chapter ${eval.name} failed")
}

		/**
		 * <p>Generic trait to name and execute a test case using Scalatest</p>
		 * @author Patrick Nicolas
		 * @since July, 13, 2014
		 * @note Scala for Machine Learning
		 */
trait Eval {
	val name: String
	
		/**
		 * <p>Execution of scalatest case.</p>
		 */
	def run(args: Array[String]): Int
}


// --------------------------  EOF -------------------------------
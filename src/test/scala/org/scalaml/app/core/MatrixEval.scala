/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97
 */
package org.scalaml.app.core

import org.scalaml.app.Eval
import org.scalaml.util.{Matrix, Display, ToString}

		/**
		 * <p><b>Purpose</b>Singleton to evaluate the implementation of a simple matrix</p>
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning chapter 1
		 */
object MatrixEval extends Eval {
	import scala.util.{Try, Success, Failure, Random}
	import org.apache.log4j.Logger
		/**
		 * Name of the evaluation 
		 */
	val name: String = "MatrixEval"
		/**
		 * Maximum duration allowed for the execution of the evaluation
		 */
	val maxExecutionTime: Int = 2000
	
	private val logger = Logger.getLogger(name)
	
			/**
		 * <p>Execution of the scalatest for <b>Matrix</b></p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		Display.show(s"$header Matrix operations", logger)
		val nCols = 5
		val nRows = 7
		val data = Array.tabulate(nCols*nRows)(_ => Random.nextDouble)
		val m = Matrix[Double](nRows, nCols, data)
		
		Try {
			Display.show(s"$name input matrix\n${m.toString}", logger)
			Display.show(s"$name Column 2: ${ToString.toString(m.col(2))}", logger)
			Display.show(s"$name Row 4   : ${ToString.toString(m.row(4))}", logger)
			Display.show(s"$name transposed matrix\n${m.transpose.toString}", logger)
		}
		match {
			case Success(n) => n
			case Failure(e) => Display.error(s"$name failed", logger, e)
		}
	}
}

// ------------------------------  EOF --------------------------
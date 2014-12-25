/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.app.core

import org.scalaml.app.Eval
import org.scalaml.core.Matrix
import org.scalaml.util.{DisplayUtils, FormatUtils}

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
		 * <p>Execution of the scalatest for <b>Matrix</b></p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Matrix operations", logger)
		val nCols = 5
		val nRows = 7
		val data = Array.tabulate(nCols*nRows)(_ => Random.nextDouble)
		val m = Matrix[Double](nRows, nCols, data)
		
		Try {
			DisplayUtils.show(s"$name input matrix\n${m.toString}", logger)
			DisplayUtils.show(s"$name Column 2: ${FormatUtils.format(m.col(2))}", logger)
			DisplayUtils.show(s"$name Row 4   : ${FormatUtils.format(m.row(4))}", logger)
			DisplayUtils.show(s"$name transposed matrix\n${m.transpose.toString}", logger)
		}
		match {
			case Success(n) => n
			case Failure(e) => failureHandler(e)
		}
	}
}

// ------------------------------  EOF --------------------------
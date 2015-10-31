/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99
 */
package org.scalaml.app.chap2

import org.scalaml.app.Eval
import org.scalaml.util.MathUtils._
import org.scalaml.util.{DisplayUtils, FormatUtils, LoggingUtils}
import org.scalaml.core.Types.ScalaMl.DblArray
import LoggingUtils._
import org.scalaml.util.Assertable

		/**
		 * '''Purpose'''Singleton to evaluate the operations on a simple matrix of
		 * elements of type Double
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see Scala for Machine Learning chapter 1 "Getting Started"
		 * @see org.scalaml.app.Eval
		 * @see org.scalaml.util.Assertable
		 */
object DMatrixEval extends Eval with Assertable {
	import scala.util.{Try, Random}
	import org.apache.log4j.Logger
		/**
		 * Name of the evaluation 
		 */
	val name: String = "DMatrixEval"
	val assertMsg: String = "DMatrixEval transpose"
	
	val expected = Array[Array[Double]](
		Array[Double](0.12, 5.6, 1.08, 0.24, 0.72, 1.2, 0.36),
		Array[Double](0.48, 5.96, 0.12, 0.6, 1.08, 0.24, 0.72),
		Array[Double](0.84, -84.8, 0.48, 0.96, 0.12, 0.6, 1.08),
		Array[Double](1.2, 5.36, 0.84, 0.0, 0.48, 0.96, 0.12),
		Array[Double](0.24, 5.72, 1.2, 0.36, 0.84, 0.0, 0.48)	    
	)
		/**
		 * Execution of the scalatest for '''Matrix'''
		 * 	 
		 * Exceptions thrown during the execution of the tests are caught by the wrapper or handler
		 * test method in Eval trait defined as follows:
		 * {{{
		 *    def test(args: Array[String]) =
		 *      Try(run(args)) match {
		 *        case Success(n) => ...
		 *        case Failure(e) => ...
		 * }}}
		 * The tests can be executed through ''sbt run'' or individually by calling 
		 * ''TestName.test(args)'' (i.e. DKalmanEval.test(Array[String]("IBM") )
		 * @param args array of arguments used in the test
		 */
	override protected def run(args: Array[String]): Int = {
		show(s"$header Matrix operations")
		
		val nCols = 5
		val nRows = 7
		val data = Array.tabulate(nCols*nRows)(n => ((3*n+1)%11)*0.12)
	
		
		val m = new DMatrix(nRows, nCols, data)
		

		show(s"Input matrix\n${m.toString}")
		val mm = m += (1, 2, -89.8)
		show(s"+= matrix\n${mm.toString}")
		m += (1, 5.0)
		show(s"Update a row matrix\n${m.toString}")
		show(s"Column 2: ${FormatUtils.format(m.col(2).toVector)}")
		val r: DblArray = m.row(4)
		show(s"Row 4   : ${r.mkString(",")}")
		
		val transposed = m.transpose
		show(s"transposed matrix\n${transposed.toString}")

		assertDblArray(transposed.data, expected.flatten, 1e-2)
		val m2 = Range(0, 2)./:(DMatrix(3, 2))((m, n) => m += (1, n, 1.0))
		show(s"Fold results\n${m2.toString}")
	}
}

// ------------------------------  EOF --------------------------
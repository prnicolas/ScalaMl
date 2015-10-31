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
package org.scalaml.app.chap1

import org.scalaml.stats.{MinMaxVector, XTSeries}
import org.scalaml.util.Assertable
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.app.Eval



		/**
		 * '''Purpose''': Singleton to validate the MinMax class
		 * 
		 * @author Patrick Nicolas
		 * @since 0.99 June 17, 2013
		 * @see Scala for Machine Learning Chapter 1 "Getting Started" Let's kick the tires / Plotting
		 * @see org.scalaml.app.Eval
		 * @see org.scalaml.util.Assertable
		 */
object MinMaxEval extends Eval with Assertable {
	/**
		 * Name of the evaluation 
		 */
	val name: String = "ETransformEval"
	protected val assertMsg: String = "MinMaxEval normalization failed"
	
	val values: XVSeries[Double] = Vector[DblArray](
		Array[Double](2.6, 1.7, 9.9),
		Array[Double](-2.9, 11.7, 29.9),
		Array[Double](0.6, -17.5, 50.5),
		Array[Double](12.0, 0.2, -34.8)
	)
	
	val expected: XVSeries[Double] = Vector[DblArray](
		Array[Double](0.3691 ,0.6575,0.5240),
		Array[Double](0.0,1.0,0.7584),
		Array[Double](0.2349,0.0,1.0),
		Array[Double](1.0,0.6061,0.0)
	)
	
	val value1 = Array[Double](2.6, 1.7, 9.9)
	val expected1 = Array[Double](0.3691,0.6575, 0.5240)
	val value2 = Array[Double](-70.9, 1.7, 90.9)
	val expected2 = Array[Double](0.0,0.6575,1.0)
	    
		/**
		 * Execution of the scalatest for'''MinMax'''.
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
		show(s"$header Evaluation eTransform monad")

 
		val minMaxes = new MinMaxVector(values)
  
		val res = minMaxes.normalize(0.0, 1.0)
		assertXVSeries(res, expected, 1e-2)
		res.map(r => show(s"Normalized data ${r.mkString(",")}"))
		
		val res1 = minMaxes.normalize(value1)
		assertDblArray(res1.get, expected1, 1e-2)
		val res2 = minMaxes.normalize(value2)
		assertDblArray(res2.get, expected2, 1e-2)
	}
}


// ------------------------------  EOF --------------------------------------------

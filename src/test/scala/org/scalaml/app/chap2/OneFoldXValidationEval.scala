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
import org.scalaml.core.Types.ScalaMl.{DblVector, DblArray}
import org.scalaml.validation.OneFoldXValidation
import org.scalaml.util.Assertable

		/**
		 * '''Purpose''' Singleton to test the one-fold cross validation class 
		 * and the random generation of training and test sets from an original
		 * set of labeled observations
		 * 
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 2 "Hello World!"
		 */
object OneFoldXValidationEval extends Eval with Assertable {
  	/**
		 * Name of the evaluation 
		 */
	val name: String = "OneFoldXValidationEval"
	protected val assertMsg: String = "One fold validation test"
	  	/**
		 * Execution of the scalatest for '''XTSeries'''
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
		 * 
		 * @param args array of arguments used in the test
		 */
	override protected def run(args: Array[String]): Int = {
		show(s"$header Generic time series")

			// A sample of observed features
		val xt = Vector[DblArray](
			Array[Double](3.5, 1.6),
			Array[Double](4.5, 2.6),
			Array[Double](5.5, 3.6),
			Array[Double](6.5, 4.6),
			Array[Double](7.5, 5.6),
			Array[Double](8.5, 6.6),
			Array[Double](9.5, 7.6),
			Array[Double](10.5, 8.6),
			Array[Double](11.5, 9.6),
			Array[Double](12.5, 10.6),
			Array[Double](13.5, 11.6),
		  Array[Double](14.5, 12.6),
			Array[Double](15.5, 13.6),
			Array[Double](16.5, 14.6),
			Array[Double](17.5, 15.6),
			Array[Double](18.5, 16.6),
			Array[Double](19.5, 17.6)
		)
 
			// sample of labels
		val expected = Vector[Int](0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1)
		assertInt(xt.size, expected.size)
  
			// Create a 1 fold validation
		val xValidation = new OneFoldXValidation[Double](xt, expected, 0.6)
		
			// extracts the training and validation sets
		val trainSet = xValidation.trainingSet.map {
			case(x, n) => s"${x.mkString(",")} | $n"
		}
		val valSet = xValidation.validationSet.map{
			case(x, n) => s"${x.mkString(",")} | $n"
		} 
			// Validate the size of training and validation set
		assertInt(trainSet.size + valSet.size, xt.size)
		show(s"Training set:\n${trainSet.mkString("\n")}")
		show(s"Validation set:\n${valSet.mkString("\n")}")
	}
}


// ----------------------  EOF -----------------------------
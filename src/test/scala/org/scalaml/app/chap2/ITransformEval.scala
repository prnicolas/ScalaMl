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

import scala.util.Try

import org.apache.log4j.Logger

import org.scalaml.app.Eval
import org.scalaml.core.{ITransform, ITransformMonad}
import org.scalaml.util.DisplayUtils


		/**
		 * '''Purpose''' Singleton to evaluate the implicit data transformation monad
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 2 "Hello world!"
		 * @see org.scalaml.app.Eval
		 */
object ITransformEval extends Eval {
  import ITransformMonad._
  		
  	/**
		 * Name of the evaluation 
		 */
	val name: String = "ITransformEval"

		/**
		 * Execution of the scalatest for'''ITransformMonad'''
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
		show(s"$header Evaluation ITransform monad")
  
			/**
			 * A simple data transformation using a model derived from a training set, x
			 * @param x training set
			 */
		class _ITransform(x: Vector[Float]) extends ITransform[Float](x) {
			type V = Double
    
			override def |> : PartialFunction[Float, Try[V]] = {
				case y: Float if(Math.abs(y) < 100) => Try(Math.exp(y))
			}
		}
  
			// instantiate the transformation
		val input = Vector[Float](1.6F, 8.0F, -0.4F)
		val _iTransform = new _ITransform(input)
  
			// Get the new ITransform
		val expected = input.map( _ * 2.0)
		val found = _iTransform.map( _ * 2.0).xt
		
		assert(found.equals(expected), 
			s"$header found ${found.mkString(",")} require ${expected.mkString(",")}")
		show(s"$name ${found.mkString(",")}")
	}
}


// ------------------------------  EOF ------------------------
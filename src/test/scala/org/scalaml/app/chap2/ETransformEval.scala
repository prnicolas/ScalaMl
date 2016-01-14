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

import scala.util.{Try, Success, Failure}

import org.apache.log4j.Logger

import org.scalaml.app.Eval
import org.scalaml.core.{ETransform, ETransformMonad}
import org.scalaml.util.DisplayUtils


		/**
		 * '''Purpose''' Singleton to evaluate explicit data transformation monad
		 * @author Patrick Nicolas
		 * @see Scala for Machine Learning Chapter 2 "Hello world!"
		 */
object ETransformEval extends Eval {
  import ETransformMonad._
  		
  	/**
		 * Name of the evaluation 
		 */
	val name: String = "ETransformEval"

		/**
		 * Execution of the scalatest for'''ETransformMonad'''
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
		show(s"$header Evaluation eTransform monad")
  
			/**
			 * A simple data transformation using an explicit configuration, n
			 * @param n configuration parameter
			 */
		final class _ETransform(n: Int) extends ETransform[Int](n) {
			type U = Int
			type V = Double
			  /**
			   * Partial function that applies a scaling factor to the input parameter
			   */
			override def |> : PartialFunction[U, Try[V]] = {
      	case u: U if u > 0 => Try(n*u)
			}
		}
  
			// instantiate the transformation
		val _eTransform = new _ETransform(5)
  
			// Get the new ETransform and apply a map function
		val result = _eTransform.map( _ + 1 ).config
		assert(result == 6, s"$name found $result require 6")
	  show(s"Map ${_eTransform.config} + 1 = $result")
	  
		  // Test the new explicit transform with the |> operator
		val input: Int = 3
		_eTransform |> (3) match {		
		  case Success(x) => {
		    assert(x == 15, s"$name found $x require 15")
		    show(s"Transform $input by ${_eTransform.config} = ${x}")
		  }
		  case Failure(e) => error(e.toString)
		}
	}
}  



// ------------------------------  EOF ------------------------
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
package org.scalaml.app

	// Scala standard library
import scala.annotation.switch
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure, Properties}
import scala.concurrent.duration.Duration
import org.scalatest.FunSuite
import org.scalatest.time._
import org.scalatest.concurrent.ScalaFutures
import org.scalaml.util.DisplayUtils
import org.scalaml.util.FormatUtils


	/**
		 * Generic template for the scalatest invocation.
		 * @author Patrick Nicolas
		 * @since July, 13, 2014
		 * @note Scala for Machine Learning
		 */
trait ScalaMlTest extends FunSuite with ScalaFutures {
	val chapter: String
	
		// Define the maximum time allowed for a Scala test to execute
	private val MAX_EXECUTION_TIME: Int = 50
	implicit protected val patience = PatienceConfig(timeout = Span(MAX_EXECUTION_TIME, Seconds), 
			interval = Span(250, Millis))
			
		/**
		 * Trigger the execution of a Scala test for a specific method and set of arguments.
		 * @param args argument for the Scala test
		 * @param method Name of the method to be tested.
		 */
	def evaluate(eval: Eval, args: Array[String] = Array.empty): Unit = {
		val f: Future[Int] = Future { eval.test(args) }
		
  		// Block until the Scala test either terminates or times out.
		whenReady(f) { result => assert(result >= 0, "OK") }
	}
}

		/**
		 * Generic trait to name and execute a test case using Scalatest
		 * @author Patrick Nicolas
		 * @since 0.98.1 July, 13, 2014
		 * @see Scala for Machine Learning
		 */
trait Eval {
	import org.apache.log4j.Logger
		/**
		 * Name of the evaluation or test 
		 */
	val name: String
	protected lazy val logger: Logger = Logger.getLogger(s"$name")
	
	
		/**
		 * Execution of scalatest case.
		 */
	def test(args: Array[String]): Int = Try (run(args) ) match {
		case Success(n) => show(s"completed")
		case Failure(e) => error(s"${e.toString}", e)
	}
	
	
	protected def run(args: Array[String]): Int
	
	protected def header: String = {
		AllTests.count += 1
		s"\n\n *****  test#${AllTests.count} $name"
	}
	
	protected def show(description: String): Int = DisplayUtils.show(s"$name $description", logger)
	
	protected def error(description: String): Int = DisplayUtils.error(s"$name $description", logger)
	
	protected def error(description: String, e: Throwable): Int = {
		DisplayUtils.error(s"$name $description", logger, e)
		e.printStackTrace
		-1
	}
	
	protected def none(description: String): Option[Int] = 
		DisplayUtils.none(s"$name $description", logger)
	

	protected def failureHandler(e: Throwable): Int = 
		if( e.getMessage != null) error(s"$name.run ${e.getMessage} caused by ${e.getCause.toString}")
		else error(s"$name.run ${e.toString}")
}

// --------------------------  EOF -------------------------------
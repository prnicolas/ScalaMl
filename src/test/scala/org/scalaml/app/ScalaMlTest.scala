/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
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


	/**
		 * <p>Generic template for the scalatest invocation.</p>
		 * @author Patrick Nicolas
		 * @since July, 13, 2014
		 * @note Scala for Machine Learning
		 */
trait ScalaMlTest extends FunSuite with ScalaFutures {
	val chapter: String
		// Define the maximum time allowed for a Scala test to execute
	private val MAX_EXECUTION_TIME: Int = 40
	implicit protected val patience = PatienceConfig(timeout = Span(MAX_EXECUTION_TIME, Seconds), 
			interval = Span(250, Millis))
			
		/**
		 * <p>Trigger the execution of a Scala test for a specific method and set of arguments.</p>
		 * @param args argument for the Scala test
		 * @param method Name of the method to be tested.
		 */
	def evaluate(eval: Eval, args: Array[String] = Array.empty): Unit = {
		val f: Future[Int] = Future { eval.run(args) }
  		// Block until the Scala test either terminates or times out.
		whenReady(f) { result => assert(result >=0, "OK") }
	}
}

		/**
		 * <p>Generic trait to name and execute a test case using Scalatest</p>
		 * @author Patrick Nicolas
		 * @since July, 13, 2014
		 * @note Scala for Machine Learning
		 */
trait Eval {
	import org.apache.log4j.Logger
		/**
		 * Name of the evaluation or test 
		 */
	val name: String
	protected lazy val logger = Logger.getLogger(this.name)
		/**
		 * <p>Execution of scalatest case.</p>
		 */
	def run(args: Array[String]): Int
	
	protected def header: String = {
		AllTests.count += 1
		s"\n\n *****  test#${AllTests.count} $name"
	}
	
	protected def failureHandler(e: Throwable): Int = e match {
		case e: MatchError => 
			DisplayUtils.error(s"$name.run ${e.getMessage} caused by ${e.getCause.toString}", logger)
		case _ => 
			DisplayUtils.error(s"$name.run ${e.toString}", logger)
	}
}

// --------------------------  EOF -------------------------------
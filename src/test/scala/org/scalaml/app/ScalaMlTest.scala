/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96a
 */
package org.scalaml.app

import org.scalatest.FunSuite
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import scala.concurrent.duration.Duration	
import akka.util.Timeout
import akka.actor.ActorSystem

		/**
		 * <p>Generic template for the scalatest invocation.</p>
		 * @author Patrick Nicolas
		 * @since July, 13, 2014
		 * @note Scala for Machine Learning
		 */
trait ScalaMlTest extends FunSuite {

	final val DURATION = 1200000
    
	val duration = Duration(DURATION, "millis")
	implicit val timeout = new Timeout(duration)
	implicit val actorSystem = ActorSystem("system") 
	val chapter: String
  
		/**
		 * <p>Trigger the execution of a Scala test for a specific method and set of arguments.</p>
		 * @param args argument for the Scala test
		 * @param method Name of the method to be tested.
		 */
	def evaluate(eval: Eval, args: Array[String] = Array.empty): Unit = {
	//	assert(eval.run(args) >= 0, s"$chapter ${eval.name} failed")
		val future = Future[Int] { eval.run(args) }
		
		Try {
			val status = Await.result(future, timeout.duration)
			status
		}
		match {
			case Success(n) => {
				if(n >= 0) 
					Console.println(s"$chapter ${eval.name} succeed with status = $n")
				actorSystem.shutdown
				assert(n >= 0, s"$chapter ${eval.name} failed")
			}
			case Failure(e) => {
				actorSystem.shutdown
				assert(false, s"$chapter ${eval.name} failed with ${timeout.duration.toString}")
			}
		}
	//	actorSystem.shutdown
	}
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

object Eval {
	
	def testCount: String = ""
	/*
	private var testCounter = 0
	
	def testCount: String = {
		testCounter += 1
		s"$testCounter"
	}
	* 
	*/
}

// --------------------------  EOF -------------------------------
/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97
 */
package org.scalaml.app

import org.scalatest.FunSuite
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure, Properties}
import scala.concurrent.duration.Duration
import akka.util.Timeout
import akka.actor.ActorSystem
import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef

import org.scalaml.util.Display

	/**
		 * <p>Generic template for the scalatest invocation.</p>
		 * @author Patrick Nicolas
		 * @since July, 13, 2014
		 * @note Scala for Machine Learning
		 */
trait ScalaMlTest extends FunSuite {
	import org.apache.log4j.Logger
	private val logger = Logger.getLogger("TestContext")

	val chapter: String
		/**
		 * <p>Trigger the execution of a Scala test for a specific method and set of arguments.</p>
		 * @param args argument for the Scala test
		 * @param method Name of the method to be tested.
		 */
	def evaluate(eval: Eval, args: Array[String] = Array.empty): Boolean = {
		Try (eval.run(args) ) match {
			case Success(n) => {
				if(n >= 0) 
					Display.show(s"$chapter ${eval.name} succeed with status = $n", logger)
				assert(n >= 0, s"$chapter ${eval.name} Failed")
				true
			}
			case Failure(e) => {
				Display.show(s"$chapter ${eval.name} ${e.getMessage}", logger)
				assert(false, s"$chapter ${eval.name} Failed")
				true
			}
		}
	}
}

		/**
		 * <p>Generic trait to name and execute a test case using Scalatest</p>
		 * @author Patrick Nicolas
		 * @since July, 13, 2014
		 * @note Scala for Machine Learning
		 */
trait Eval {
  		/**
		 * Name of the evaluation 
		 */
	val name: String
		/**
		 * Maximum duration allowed for the execution of the evaluation
		 */
	val maxExecutionTime: Int
		/**
		 * <p>Execution of scalatest case.</p>
		 */
	def run(args: Array[String]): Int
}


object Eval {
	var count: Int = _
	def testCount: String = {
		count += 1
		String.valueOf(count)
	}
}


		/**
		 * Singleton that defines the actor context for
		 * all the tests. In the case of sbt test:run execution, the
		 * context is initialized only once before execution of all the tests.
		 * and shutdown after the execution of all the tests.
		 */
object TestContext { 
	import org.apache.log4j.Logger
	private val logger = Logger.getLogger("TestContext")
	
	var allRuns = false
	lazy val actorSystem = ActorSystem("System") 

	private val ELAPSE_TIME = 4000
	
		/**
		 * Method to validate the version of Scala and Java JDK used.
		 */
	def init: Unit = {
		Display.show(s"TestContext.init\nUser: ${Properties.userName}, OS: ${Properties.osName}", logger)
		if( !Properties.isWin && !Properties.isMac)
			Display.show("The library has not be tested for this Operating System", logger)
			
		Display.show(s"Java version: ${Properties.javaVersion}", logger)
		if(!Properties.isJavaAtLeast("1.7"))
			Display.show("Incompatible version of Java, should be 1.7 or later", logger)
			
		val scalaVersion = Properties.versionNumberString
		Display.show(s"Scala version: $scalaVersion", logger)
		scalaVersion.charAt(2) match {
			case '9' => Display.show("Scala version should be 2.10.2 or higher", logger)
			case '1' => {
				scalaVersion.charAt(3) match {
					case '0' => Display.show("Compatible Akka version should be 2.2.3 or lower", logger)
					case '1' => Display.show("Compatible Akka version should be 2.3.4 or higher", logger)
				}
			}
			case _ => Display.show("Could not initialize", logger)
		}
		allRuns = true
		Eval.count = 0
	}
	
		/**
		 * Shutdown in-conditionally the Akka actor system, after a
		 * specific amount of time. This function is called by test:run
		 */
	def shutdownAll: Unit = {
		Thread.sleep((ELAPSE_TIME<<1))
		actorSystem.shutdown
		Display.show(s"test:run completed after ${Eval.count +1} tests", logger)
	}
	
		/**
		 * Shutdown the Akka actor system for individual scalatest
		 * @return 0 for success
		 */
	def shutdown: Int = {
		Thread.sleep(ELAPSE_TIME)
		if( !allRuns )
			actorSystem.shutdown
		0
	}
}


// --------------------------  EOF -------------------------------
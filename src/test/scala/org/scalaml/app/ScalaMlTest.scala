/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
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
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import scala.concurrent.duration.Duration
import akka.util.Timeout
import akka.actor.ActorSystem
import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef

	/**
		 * <p>Generic template for the scalatest invocation.</p>
		 * @author Patrick Nicolas
		 * @since July, 13, 2014
		 * @note Scala for Machine Learning
		 */
trait ScalaMlTest extends FunSuite {
	val chapter: String

		/**
		 * <p>Trigger the execution of a Scala test for a specific method and set of arguments.</p>
		 * @param args argument for the Scala test
		 * @param method Name of the method to be tested.
		 */
	def evaluate(eval: Eval, args: Array[String] = Array.empty): Boolean = {
	  
	  /*
		implicit val actorSystem = ActorSystem("system")
  		var status: Int = INITIAL_STATUS
	
	  		// Anonymous Akka actor that wraps the execution of the scala test.
		val worker = actorSystem.actorOf(Props(new Actor {
			def receive = { 
				case msg: String => {
					status = eval.run(args)
					context.stop(self)
				}
			}
		}))
		var errorMsg = "failed"

		
			// The main thread blocks until either the maxExecutionTime is reached
			// of the computation status has been updated...
		Try {
			worker ! "Start"
			val startTime = System.currentTimeMillis
			while( status == INITIAL_STATUS) {
				Thread.sleep(200)
				
				// Exit if time out 'maxExecutionTime" is exceeded
				if(System.currentTimeMillis - startTime > maxExecutionTime) {
					status = TIMEOUT
					errorMsg = s"time out after $maxExecutionTime msecs."
				}
			}
			status
		}
		* 
		*/
		Try (eval.run(args) ) match {
			case Success(n) => {
				if(n >= 0) 
					Console.println(s"$chapter ${eval.name} succeed with status = $n")
				assert(n >= 0, s"$chapter ${eval.name} Failed")
				true
			}
			case Failure(e) => {
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
	val name: String
	val maxExecutionTime: Int
	
		/**
		 * <p>Execution of scalatest case.</p>
		 */
	def run(args: Array[String]): Int
}

object Eval {
	var count = 0
	def testCount: String = {
		count += 1
		String.valueOf(count)
	}
			
}

/*
object MyEval extends Eval {
  val name: String = "Hello"
  def run(args: Array[String]): Int = {
    println("Start run")
    	Thread.sleep(4000)
    	println("End run")
    	1
  }
}

object MyScalaMlTest extends ScalaMlTest {
  val maxExecutionTime: Int = 10000
  val chapter: String  = "CC"
	def test {
		evaluate(MyEval)
	}
}

object MyApp extends App {
	MyScalaMlTest.test
	println("End")
	
}
* 
*/

// --------------------------  EOF -------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 */
package org.scalaml.app

import org.scalatest.FunSuite
import scala.concurrent.{Future, Await}

		/**
		 * <p>Generic template for the scalatest invocation.</p>
		 * @author Patrick Nicolas
		 * @since July, 13, 2014
		 * @note Scala for Machine Learning
		 */
trait ScalaMlTest extends FunSuite {
	import scala.concurrent.ExecutionContext.Implicits.global
	import scala.util.{Try, Success, Failure}
	import scala.concurrent.duration.Duration	
	import akka.util.Timeout
	import akka.actor.ActorSystem

	val duration = Duration(12000, "millis")
	implicit val timeout = new Timeout(duration)
	implicit val actorSystem = ActorSystem("system") 
	val chapter: String
  
		/**
		 * <p>Trigger the execution of a Scala test for a specific method and set of arguments.</p>
		 * @param args argument for the Scala test
		 * @param method Name of the method to be tested.
		 */
	def evaluate(eval: Eval, args: Array[String] = Array.empty): Unit = {
		val future = Future[Int] { eval.run(args) }
		
		Try {
			val status = Await.result(future, timeout.duration)
			actorSystem.shutdown
			status
		}
		match {
			case Success(n) => {
				if(n >= 0) {
					println(s"$chapter ${eval.name} succeed with status = $n")
				}
				assert(n >= 0, s"$chapter ${eval.name} failed")
			}
			case Failure(e) => assert(false, s"$chapter ${eval.name} failed with ${timeout.duration.toString}")
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
	
		/**
		 * <p>Execution of scalatest case.</p>
		 */
	def run(args: Array[String]): Int
}

/*
object MyEval extends Eval {
    val name: String = "MyEval"
	override def run(args: Array[String]): Int =  {
		println("Start eval")
		Thread.sleep(1000)
		println("end eval")
		1
	}
}

object MyTest extends ScalaMlTest {
  	val chapter: String = "MyChapter"
  	def test1 = evaluate(MyEval, Array.empty)
}

object ScalaMlTestApp extends App {
	MyTest.test1
	println("Completed")
}
* 
*/


// --------------------------  EOF -------------------------------
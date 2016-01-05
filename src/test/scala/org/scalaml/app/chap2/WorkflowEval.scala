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

import scala.util.{Try, Random}
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import org.apache.log4j.Logger

import org.scalaml.stats.{MinMax, BiasVariance}
import org.scalaml.workflow._
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.core.ETransform
import org.scalaml.util.{DisplayUtils,  LoggingUtils}
import LoggingUtils._
import org.scalaml.app.Eval




	/**
	 * Singleton to evaluate the Dependency injection based workflow class.
	 * 
	 * @author Patrick Nicolas
	 * @since February 3, 2014
	 * @note Scala for Machine Learning
	 */
object WorkflowEval extends Eval {
	import scala.util.{Try, Success, Failure}
  		/**
		 * Name of the evaluation 
		 */
	val name: String = "WorkflowEval"
	
		/**
		 * Execution of the scalatest for dependency injection. 
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate.
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
		 * @param args array of arguments used in the test
		 */
	override protected def run(args: Array[String]): Int = {
		show(s"$header Evaluation for 'log(1+x) + noise' transform")
		
		val samples = 100
		val normRatio = 10
		val splits = 4
					
		val g = (x: Double) => Math.log(x + 1.0) + Random.nextDouble
		
		val workflow = new Workflow[Int, Double => Double, DblVector, DblVector, Int] 
							with Sampling[Int, Double => Double, DblVector] 
								with Normalization[Int, DblVector, DblVector] 
									with Aggregation[Int, DblVector, Int] {
			
			val sampler = new ETransform[Int](samples) {
				type U = Double => Double
				type V = DblVector
	
				override def |> : PartialFunction[U, Try[V]] = { 
					case f: U => Try {
						val sampled = Vector.tabulate(samples)(n => f(n.toDouble/samples))
						show(s"sampling : ${sampled.mkString(",")}")
						sampled
					}
				}
			}
				
			val normalizer = new ETransform[Int](normRatio) {
				type U = DblVector
				type V = DblVector
  
				override def |> : PartialFunction[U, Try[V]] = { 
					case x: U if x.nonEmpty => Try {
						val minMax = MinMax[Double](x).map(_.normalize(0.0, 1.0)).getOrElse(Vector.empty[Double])
						show(s"normalization : ${minMax.mkString(",")}")
						minMax
					}
				}
			}

			val aggregator = new ETransform[Int](splits) {
				type U = DblVector
				type V = Int
  
				override def |> : PartialFunction[U, Try[V]] = {
					case x: U if x.nonEmpty => Try {
						show("aggregation")
						Range(0, x.size).find(x(_) == 1.0).getOrElse(-1)
					}
				}
			}
		}
		(workflow |> g) match {
			case Success(res) => show(s"WorkflowEval result = ${res.toString}")
			case Failure(e) => error(s"WorkflowEval", e)
		}
	}
}

// -------------------------------------  EOF -----------------------------------------
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
package org.scalaml.app.chap1

import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.plots.Legend
import org.scalaml.core.Types
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.supervised.regression.logistic.LogBinRegression
import org.scalaml.app.Eval
import org.scalaml.stats.XTSeries._
import LogBinRegression._

		/**
		 * '''Purpose'''Singleton to evaluate a simple implementation of a 
		 * two class logistic regression for a single variable
		 * @author Patrick Nicolas
		 * @since 0.97  November 11, 2013
		 * @version 0.98.2
		 * @see Scala for Machine Learning chapter 1 ''Getting Started'' / Let's kick the tires
		 */
object LogBinRegressionEval2 extends Eval {
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "LogBinRegressionEval2"
		
	private val NITERS = 5000
	private val EPS = 1e-4
	private val ETA = 0.02
	
		/**
		 * Execution of the scalatest for two simple validations of the '''LogBinRegression''' class. 
		 * This method is invoked by the actor-based test framework function, ScalaMlTest.evaluate.
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
	override protected def run(args: Array[String]): Int =  {
		show(s"$header binomial logistic regression test")
		
			// Input data (2 dimension)
		val x1 = Vector[DblArray](
			Array[Double](0.1, 0.18),
			Array[Double](0.21, 0.17),
			Array[Double](0.45, 0.39),
			Array[Double](0.68, 0.09),
			Array[Double](0.85, 0.01),
			Array[Double](0.87, 0.73),
			Array[Double](0.59, 0.63),
			Array[Double](0.67, 0.21)
		)
			// Unormalized expected values
		val y1 = Vector[Double](1.01, 1.06, 2.49, 1.09, 0.9, 4.58, 3.81, 1.66)
		
			// Input data (single dimension)
		val x2 = Vector[DblArray](
			Array[Double](0.1),
			Array[Double](0.21),
			Array[Double](0.13),
			Array[Double](0.89),
			Array[Double](0.58),
			Array[Double](0.87),
			Array[Double](0.02),
			Array[Double](0.42),
			Array[Double](0.0)
		)
			// Unormalized expected values
		val y2 = Vector[Int](1, 2, 1, 9, 6, 9, 0, 4, 0)
		
			// Implicit conversion from Array[Double] to Double
	//	implicit val reduce = (w: Array[Double]) => w(0)
		
		
		
		
			// Generation of a binomial logistic model for a two variable model
		normalize(y1.map(_.toDouble)) match {
			case Success(yn1) =>
				val regr = new LogBinRegression(x1, yn1, NITERS, ETA, EPS)
								
				val res = regr.counters(COUNT_ERR).map( _.map(_.toString).mkString("\n"))
						.getOrElse("Empty data")
				
				show(s"Counter dump $res")
				val legend = Legend("Weights", 
						"Binomial logistic regression convergence", "Recursions", "Error")
				regr.display(List[String]("w0", "w1"), legend)

				val test0 = Array[Double](0.23, 0.19)
				show(s"${regr.classify(test0)}")
				val test1 = Array[Double](0.07, 0.71)
				show(s"${regr.classify(test1)}")

			case Failure(e) => error("Binomial logistic regression test 2- dimension", e)
		}
		
			// Generation of a binomial logistic model for a single variable model
		normalize(y2.map(_.toDouble)) match {
			case Success(yn2) =>
				val regr = new LogBinRegression(x2, yn2, NITERS, ETA, EPS)
				val res = regr.counters(COUNT_ERR).map( _.map(_.toString).mkString("\n"))
						.getOrElse("Empty data")
				
				show(s"Counter dump $res")
				val legend = Legend(COUNT_ERR, 
						"Binomial logistic regression convergence", "Recursions", "Weights")
				regr.display(COUNT_ERR, legend)

				val test0 = Array[Double](0.09)
				show(s"${regr.classify(test0)}")
				val test1 =  Array[Double](0.91)
				show(s"${regr.classify(test1)}")

			case Failure(e) => error("Binomial logistic regression test single dimension", e)
		}
	}
}

// --------------------  EOF --------------------------------------
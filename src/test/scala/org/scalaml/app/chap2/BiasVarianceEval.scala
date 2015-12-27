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
import org.apache.log4j.Logger
import org.scalaml.stats.{Stats, BiasVariance}
import org.scalaml.core.Types.ScalaMl
import org.scalaml.util.{FormatUtils, DisplayUtils, LoggingUtils}
import org.scalaml.app.Eval
import LoggingUtils._, FormatUtils._
import org.scalaml.util.Assertable

		/**
		 * '''Purpose'''Singleton to evaluate the bias-variance trade-off
		 * using synthetically generated data
		 * 
		 * @author Patrick Nicolas
		 * @version 0.98.1
		 * @see Scala for Machine Learning chapter 2 "Hello World!" Bias-Variance decomposition
		 * @see org.scalaml.app.Eval
		 * @see org.scalaml.util.Assertable
		 */
object BiasVarianceEval extends Eval with Assertable {

	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "BiasVarianceEval"
	val assertMsg: String = "BiasVarianceEval predictor"
	  
	final val biasExpected = Array[Double]( 904.4,401.9)
	final val varianceExpected = Array[Double]( 1184.8,789.9)
	
		/**
		 * Execution of the scalatest for Bias Variance  decomposition. 
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate
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
		show(s"$header Evaluation of Bias Variance decomposition")

	
			
			// Any model that replicates the training set will overfit
		val training = (x: Double) => {
		  val r1 = 0.45*(Random.nextDouble-0.5)
		  val r2 = 38.0*(Random.nextDouble - 0.5)
		  0.2*x*(1.0 + Math.sin(x*0.1 + r1)) + Math.sin(x*0.3) + r2
		  
		}
			// Our target model used for emulating the validation data set
		val target = (x: Double) => 0.2*x*(1.0 + Math.sin(x*0.1))
		
			// A list of models candidates including the overfitting model that 
			/// match the training set.
		val models = List[(Double=>Double, String)] (
		    ((x: Double) => 0.2*x*(1.0 + 0.25*Math.sin(x*0.1)), "Underfitting1" ),
			  ((x: Double) => 0.2*x*(1.0 + 0.5*Math.sin(x*0.1)), "Underfitting2" ),
			  ((x: Double) => training(x), "Overfitting"),
			  ((x: Double) => 0.2*x*(1.0 + Math.sin(x*0.1)), "Fitting" )
		)
		show(s"Plot for Bias Variance decomposition")

			// Uses jFreeChart to display the test data and the three models.
		display(models, training)
		
		val bv = BiasVariance(target, 160).fit(models.map( _._1))
		val mv = bv.unzip
		println(s"${mv._1.mkString(",")}")
				println(s"${mv._2.mkString(",")}")
		val result = format(bv.toVector, "Variance", "bias", SHORT)
		
		assertDblArray(mv._1.toArray.take(2), biasExpected, 1.0)
		assertDblArray(mv._2.toArray.take(2), varianceExpected, 1.0)
		
		show(s"Result variance bias\n$result")
	}
	
	private def display(estF: List[(Double =>Double, String)], f: Double =>Double): Boolean = {
		import org.scalaml.plots.{LinePlot, BlackPlotTheme, LightPlotTheme, Legend}
		import ScalaMl._
		
		val data: List[(DblVector, String)] = (Vector.tabulate(160)( f(_)), "f") :: 
				estF./:(List[(DblVector, String)]())((xs, g) => 
						(Vector.tabulate(160)(y => g._1( y.toDouble)), g._2) :: xs)
						
		val labels = Legend( name, "Bias and Variance: Over-under fitting", "x-values", "y-values")
		LinePlot.display(data, labels, new LightPlotTheme)
	}
}


// -------------------------------------  EOF -----------------------------------------
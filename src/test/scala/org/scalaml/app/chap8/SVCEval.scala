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
package org.scalaml.app.chap8

import scala.language.postfixOps

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.Fundamentals
import org.scalaml.supervised.svm.{SVMConfig, SVM, SVMExecution}
import org.scalaml.supervised.svm.formulation._
import org.scalaml.supervised.svm.kernel._
import org.scalaml.stats.XTSeries
import org.scalaml.core.Types.ScalaMl.{DblMatrix, DblArray}
import org.scalaml.util.{DisplayUtils,  LoggingUtils}
import org.scalaml.app.Eval
import LoggingUtils._, XTSeries.Transpose._


		/**
		 * '''Purpose:''' Singleton for the evaluation of the support vector machine classifier 
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning  Chapter 8 Kernel models and support vector machines.
		 */
object SVCEval extends Eval {
	import scala.util.Try
	import org.apache.log4j.Logger
	import Fundamentals._, SVMConfig._
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "SVCEval"
	
	private val path = "resources/data/chap8/dividends2.csv"	
	private val C = 1.0
	private val GAMMA = 0.5
	private val EPS = 1e-2
	private val NFOLDS = 2

		/** Execution of the scalatest for '''SVC''' class.
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
		show(s"$header Binary Support Vector Classifier")

		val extractor = relPriceChange :: 
						debtToEquity ::
						dividendCoverage ::
						cashPerShareToPrice ::
						epsTrend ::
						shortInterest :: 
						dividendTrend :: 
						List[Array[String] =>Double]()
						
		val pfnSrc = DataSource(path, true, false, 1) |>
		val config = SVMConfig(new CSVCFormulation(C), 
									new RbfKernel(GAMMA), SVMExecution(EPS, NFOLDS))
									
			/**
			 * Generate the matrix of observations by feature from the data 
			 * extracted from the data source (CSV file)
			 */
		def getObservations(input: Vector[DblArray]): Try[Vector[DblArray]] = Try {
			val vec = input.dropRight(1).map(_.toArray)
			transpose(vec).toVector
		}
	
		(for {
			input <- pfnSrc(extractor)
			obs <- getObservations(input)
			svc <- SVM[Double](config, obs, input.last.toVector)
		} yield {
			show(s"${svc.toString}\naccuracy ${svc.accuracy.get}")	
		})
		.getOrElse( error("SVCEval failed"))
	}
}


// --------------------------  EOF -----------------------------------------------
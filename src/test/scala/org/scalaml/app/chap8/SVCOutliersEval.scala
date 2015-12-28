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

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.Fundamentals
import org.scalaml.stats.Transpose
import org.scalaml.core.Types
import org.scalaml.supervised.svm.{SVM, SVMConfig, SVMExecution}
import org.scalaml.supervised.svm.kernel.RbfKernel
import org.scalaml.supervised.svm.formulation.OneSVCFormulation
import org.scalaml.util.{DisplayUtils,  LoggingUtils}
import org.scalaml.app.Eval
import LoggingUtils._, Transpose._


		/**
		 * '''Purpose:'''Singleton to evaluate the on-class SVM for anomalies or outliers 
		 * detection.
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 8 ''Kernel Models and Support Vector Machines''.
		 */
object SVCOutliersEval extends Eval {
	import scala.util.Try
	import org.apache.log4j.Logger
	import Fundamentals._, Types.ScalaMl._
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "SVCOutliersEval"
	
	val path = "resources/data/chap8/dividends2.csv"	

	private val NU = 0.2
	private val GAMMA = 0.5
	private val EPS = 1e-3
	private val NFOLDS = 2
	
		/** Execution of the scalatest for the one-class '''SVC''' for outliers.
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
		 * 
		 * @param args array of arguments used in the test
		 */
	override protected def run(args: Array[String]): Int = {
		import scala.language.postfixOps
	  
		show(s"$header Evaluation of One class Support Vector Classifier")
		
			// Specifies the list of fundamental corporate financial metrics to
			// be extracted from the data source 
		val extractor = relPriceChange :: 
						debtToEquity ::
						dividendCoverage ::
						cashPerShareToPrice ::
						epsTrend ::
						dividendTrend :: List[Array[String] =>Double]()
	 
				// filter to distinguish null and non-null values
		val filter = (x: Double) => if(x == 0) -1.0 else 1.0
		val pfnSrc = DataSource(path, true, false, 1) |>
			
			// Convert input data into a observations x features matrix
		def getObservations(input: Vector[DblArray]): Try[Vector[DblArray]] = Try {
			val vec = input.dropRight(1).map(_.toArray)
			transpose(vec).toVector
		}
		
			// Configuration specific to the One-class Support vector classifier using 
			// the radius basis functions kernel
		val config = SVMConfig(
				new OneSVCFormulation(NU), 
				new RbfKernel(GAMMA), 
				SVMExecution(EPS, NFOLDS))
	
		(for {
				// Apply the extractor
			input <- pfnSrc(extractor)
			
				// Build the labeled input data
			obs <- getObservations(input)
			
				// Train the SVM model
			svc <- SVM[Double](config, obs, input.last.map(filter(_)).toVector)
		} yield {
			show(s"${svc.toString}\naccuracy ${svc.accuracy.get}")	
		})
		.getOrElse( error("SVCEval failed"))
	}
}


// --------------------------  EOF -----------------------------------------------
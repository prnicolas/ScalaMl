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

import org.scalaml.supervised.svm.{SVMConfig, SVM, SVMExecution}
import org.scalaml.supervised.svm.formulation.CSVCFormulation
import org.scalaml.supervised.svm.kernel.RbfKernel
import org.scalaml.stats.XTSeries
import org.scalaml.core.Types.{ScalaMl, emptyString}
import org.scalaml.util.{FormatUtils, DisplayUtils,  LoggingUtils}
import org.scalaml.app.Eval
import FormatUtils._

		/**
		 * Singleton to evaluate the impact of margin value on =
		 * on the accuracy of the classification of a binary support vector
		 * classifier using synthetic features. The synthetic values are generated
		 * using a combination of random generators. 
		 * @author Patrick Nicolas
		 * @version 0.98.2
		 * @see Scala for Machine Learning Chapter 8 ''Kernel Models and Support Vector Machines''
		 */
object SVCMarginEval extends Eval {
	import scala.util.{Random, Try}
	import org.apache.log4j.Logger
	import XTSeries._, ScalaMl._
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "SVCMarginEval"
	
	private val GAMMA = 0.8
	private val CACHE_SIZE = 1<<8
	private val NFOLDS = 2
	private val EPS = 1e-5
	val N = 100	
	private var status: Int = 0
    

		/** Execution of the scalatest for evaluating margin in '''SVC''' class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate
		 * {{{
		 * Main evaluation routine that consists of two steps
		 *    - Generation of synthetic features
		 *    - Computation of the margin for a specific C penalty value
		 * 	
		 * Exceptions thrown during the execution of the tests are caught by the wrapper or handler
		 * test method in Eval trait defined as follows:
		 * 
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
		show(s"$header Evaluation of impact of C penalty on margin")
		
		generate.map( values => {
			val result = (0.1 until 5.0 by 0.1)
					.flatMap( evalMargin(values._1, values._2, _) ).mkString("\n")
					
			show(s"\nMargin for SVC with\nC\tMargin\n$result")
		}).getOrElse(-1)
	}

		/**
		 * Generate the synthetic observations and labels (expected values)
		 */
	private def generate: Option[(Vector[DblArray], DblVector)] = {
		val z  = Vector.tabulate(N)(i =>
			Array[Double](i, i*(1.0 + 0.2*Random.nextDouble))
		) ++
		Vector.tabulate(N)(i => Array[Double](i, i*Random.nextDouble))
		
		normalize(z).map( (_, Vector.fill(N)(1.0) ++ Vector.fill(N)(0.0))).toOption
	}

		/**
		 * Evaluate the impact of the margin factor C over the accuracy of the SVM classifier
		 */
	private def evalMargin(
			features: Vector[DblArray], 
			labels: DblVector, 
			c: Double): Option[String] = {
	  
			// Set up the configuration
		val execEnv = SVMExecution(CACHE_SIZE, EPS, NFOLDS)
		val config = SVMConfig(new CSVCFormulation(c), new RbfKernel(GAMMA), execEnv)
		
			// Instantiate the SVM classifier and train the model
		val svc = SVM[Double](config, features, labels)
			
			// Extract and stringize the margin for a given C penalty value.
		svc.margin.map(_margin => 
			s"${c.floor}\t${format(_margin, emptyString, SHORT)}"
		)
	}
}

// --------------------------- EOF --------------------------------------------------
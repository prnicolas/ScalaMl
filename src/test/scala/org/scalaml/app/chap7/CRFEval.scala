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
package org.scalaml.app.chap7

import org.scalaml.util.MathUtils._
import org.scalaml.supervised.crf.{CrfConfig, Crf}
import org.scalaml.libraries.crf.CrfAdapter
import org.scalaml.util.{DisplayUtils, FormatUtils, LoggingUtils}
import org.scalaml.app.Eval
import org.scalaml.core.Types.emptyString
import org.scalaml.core.Types.ScalaMl.DblArray
import LoggingUtils._, FormatUtils._, CrfAdapter._

		/**
		 * '''Purpose:'''Singleton for the evaluation of Conditional Random Fields
		 * for sentiment analysis.
		 * @author Patrick Nicolas
		 * @version 0.98.3
		 * @see Scala for Machine Learning  Chapter 7 ''Sequential data models'' Conditional Random Fields.
		 */
object CrfEval extends Eval {
	import scala.util.Try
	import org.apache.log4j.Logger
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "CrfEval"
	
	private val LAMBDA = 0.5
	private val NLABELS = 9
	private val MAX_ITERS = 100
	private val W0 = 0.7
	private val EPS = 1e-3
	private val PATH = "resources/data/chap7/rating"

		/**
		 * Execution of the scalatest for '''Crf''' class.
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
		show(s"$header Conditional Random Fields for sentiment analysis")

			// Step 1:  create a configuration and sequence delimiter
		val config = CrfConfig(W0 , MAX_ITERS, LAMBDA, EPS)
		val delimiters = new CrfSeqDelimiter(",\t/ -():.;'?#`&_", "//", "\n")

			// Step 2: Create a CRF model (weights) through training by instantiating
			// 				 the Crf class
		
		val crf = Crf(NLABELS, config, delimiters, PATH)
		
			// Step 3: Display the model for the Crf classifier
		if(crf.weights != None ) {
			val results = crf.weights.map( w => {
				display(w)
				format(w, "CRF weights", SHORT)
			})
			show(s"weights for conditional random fields\n${results.mkString(",")}") 
		}
		else
			error("Failed to train the conditional random field")
	}
	
	
		/**
		 * Display the model parameters (or weights) on a line plot
		 */
	private def display(w: DblArray): Unit = {	
		require(w.length >0, "CRFEval.display undefined set of weights")
		
		import org.scalaml.plots.{LightPlotTheme, LinePlot, Legend}
	  
		val labels = Legend( 
			name, "Conditional random fields weights vs lambda", "Lambda distribution", "weights"
		)					
		LinePlot.display(w, labels, new LightPlotTheme)
	}
}

// --------------------------------  EOF -------------------------------
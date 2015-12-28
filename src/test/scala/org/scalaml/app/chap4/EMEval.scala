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
package org.scalaml.app.chap4

import scala.util.Try
import org.apache.log4j.Logger

import org.scalaml.stats.XTSeries
import org.scalaml.core.Types.ScalaMl.{DblArray, DblMatrix, DblVector}
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.DataSource
import org.scalaml.unsupervised.em.MultivariateEM
import org.scalaml.filtering.movaverage.SimpleMovingAverage
import org.scalaml.util.{DisplayUtils,  LoggingUtils}
import org.scalaml.app.Eval

import LoggingUtils._, DisplayUtils._,SimpleMovingAverage._, MultivariateEM._, YahooFinancials._



		/**
		 * '''Purpose:'''Singleton to  evaluate the Expectation-Maximization algorithm
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see org.scalaml.unsupervised.em
		 * @see Scala for Machine Learning Chapter 4 ''Unsupervised learning'' / 
		 * Expectation-Maximization
		 */
object EMEval extends UnsupervisedLearningEval {
 
		/**
		 * Name of the evaluation 
		 */
	val name: String = "EMEval"

		/**
		 * Execution of the scalatest for '''MultivariateEM''' class
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
		import scala.language.postfixOps
	   
		require( args.length > 0, s"$name Cannot evaluate EM with undefined arguments")
		
		show(s"$header Expectation-Maximization with ${args(0)} clusters")
     
		val K = args(0).toInt
		val samplingRate = args(1).toInt
		val period = 8
		val smAve = SimpleMovingAverage[Double](period)
        
			// extracts the observations from a set of csv files.
		assert(symbolFiles.size > 0, s"$name.run Symbol files are undefined")
			
			// Retrieve the partial function for the simple moving average
		val pfnSmAve = smAve |>
			
			// Walk through the stock ticker symbols and load the historical data from each
			// ticker symbol using the data source extractor
		val obs = symbolFiles.map(sym => {
  
			val pfnSrc = DataSource(sym, path, true, 1) |>
			
			(for {
					// Extract data from the files containing historical financial data
				xs <- pfnSrc(extractor)
				
					// Apply the simple moving average of the first variable
				if pfnSmAve.isDefinedAt(xs.head.toVector)
					values <- pfnSmAve(xs.head.toVector)
					
					// Generate the features by filtering using a sampling rate
				y <- filter(period, values, samplingRate)
			} yield y).get
		})
		profile(obs.toVector)
		em(K, obs)
	}
	
	private def filter(period: Int, values: DblVector, samplingRate: Int) = Try { 
		values.view.zipWithIndex.drop(period+1).toVector
				.filter( _._2 % samplingRate == 0)
				.map( _._1).toArray
	}

	private def profile(xv: Vector[DblArray]) {
		import org.scalaml.plots.Legend
		
		val legend = Legend("Density", "Expectation-Maximiation convergence K=6", "Iterations", "Density")
		val em = MultivariateEM[Double](3, xv)
		em.display("Density", legend)
	}
	
	
	private def em(K: Int, obs: DblMatrix): Int = {
		val em = MultivariateEM[Double](K, obs.toVector)
		show(s"${em.toString}")
	}
}


// -----------------------------------  EOF ---------------------------------------------------
/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.app.chap4

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl.{DblVector, DblMatrix}
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.DataSource
import org.scalaml.unsupervised.em.MultivariateEM
import org.scalaml.filtering.SimpleMovingAverage
import org.scalaml.util.DisplayUtils
import org.scalaml.app.Eval


		/**
		 * <p><b>Purpose:</b>Singleton to  evaluate the Expectation-Maximization algorithm
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning Chapter 4 Unsupervised learning / Expectation-Maximization
		 */
object EMEval extends UnsupervisedLearningEval {
	import scala.util.{Try, Success, Failure}
	import org.apache.log4j.Logger
	import SimpleMovingAverage._, MultivariateEM._, YahooFinancials._
 
		/**
		 * Name of the evaluation 
		 */
	val name: String = "EMEval"

		/**
		 * <p>Execution of the scalatest for <b>MultivariateEM</b> class
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	override def run(args: Array[String]): Int = {
		require( !args.isEmpty, s"$name Cannot evaluate EM with undefined arguments")
		DisplayUtils.show(s"$header Expectation-Maximization with ${args(0)} clusters", logger)
     
		val K = args(0).toInt
		val samplingRate = args(1).toInt
		val period = 8
		val smAve = SimpleMovingAverage[Double](period)
        
			// extracts the observations from a set of csv files.
		Try {
			require(symbolFiles.size > 0, s"$name.run Symbol files are undefined")
	     
			val obs: DblMatrix = symbolFiles.map(sym => {
				val xs = DataSource(sym, path, true) |> extractor
				val values: XTSeries[Double] = (XTSeries.|>(xs)).head  // force a data type conversion
	
				val filtered = smAve |> values
				filtered.zipWithIndex
						.drop(period+1)
						.toArray
						.filter( _._2 % samplingRate == 0)
						.map( _._1)
			})

			obs.find(_.isEmpty).map(_ => 
				DisplayUtils.error(s"$name.run Some observations are corrupted", logger)
			).getOrElse( {
				val components = MultivariateEM[Double](K) |> XTSeries[DblVector](obs)
				components.foreach( x => {
					DisplayUtils.show(s"\n$name value: ${x._1}\n$name Means: ", logger)
					DisplayUtils.show(x._2.toSeq, logger)
					DisplayUtils.show(s"$name Standard Deviations", logger)
					DisplayUtils.show(x._3.toSeq, logger)
				})
				DisplayUtils.show(s"$name completed", logger)
			})
		} 
		match {
			case Success(n) => n
			case Failure(e) =>  failureHandler(e)
		}
	}
}

// -----------------------------------  EOF ---------------------------------------------------
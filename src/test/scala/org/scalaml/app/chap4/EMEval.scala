/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.app.chap4

import org.scalaml.core.XTSeries
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.{DataSource, DataSink}
import YahooFinancials._
import org.scalaml.core.Types.ScalaMl._
import scala.util.{Try, Success, Failure}
import org.scalaml.util.Display
import org.apache.log4j.Logger
import org.scalaml.app.Eval



		/**
		 * <p>Object to evaluate the Expectation-Maximization algorithm
		 */
object EMEval extends UnsupervisedLearningEval {
	import org.scalaml.unsupervised.em.MultivariateEM
	import org.scalaml.filtering.SimpleMovingAverage
	import SimpleMovingAverage._
	import MultivariateEM._
    
	val name: String = "EMEval"
	private val logger = Logger.getLogger(name)

		/**
		 * <p>Execution of the scalatest for <b>MultivariateEM</b> class
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	override def run(args: Array[String]): Int = {
		require(args != null && args.length == 2, s"$name Cannot evaluate EM with undefined arguments")
		Display.show(s"\n** test#${Eval.testCount} $name Evaluation of Expectation-Maximization clustering", logger)
     
		val K = args(0).toInt
		val samplingRate = args(1).toInt
		val period = 8
		val smAve = SimpleMovingAverage[Double](period)
        
			// extracts the observations from a set of csv files.
		Try {
			require(symbolFiles.size > 0, s"$name.run Symbol files are undefined")
	     
			val obs: DblMatrix = symbolFiles.map(sym => {
				val xs = DataSource(sym, path, true) |> extractor
				val values: XTSeries[Double] = (XTSeries.|>(xs)).head  // force a data type conversion (implicit)
	
				val filtered = smAve |> values
				filtered.zipWithIndex
						.drop(period+1)
						.toArray
						.filter( _._2 % samplingRate == 0)
						.map( _._1)
			})
	     
				// If all the observations are valid
			if( obs.find( _ == Array.empty) == None) {  	 
				val components = MultivariateEM[Double](K) |> XTSeries[DblVector](obs)
				components.foreach( x => {
					Display.show(s"\n$name value: ${x._1}\n$name Means: ", logger)
					Display.show(x._2.toSeq, logger)
					Display.show(s"$name Standard Deviations", logger)
					Display.show(x._3.toSeq, logger)
				})
				Display.show(s"$name completed", logger)
			}
			else 
				Display.error(s"$name.run Some observations are corrupted", logger)
		} 
		match {
			case Success(n) => n
			case Failure(e) => Display.error(s"$name.run EM failed", logger, e)
		}
	}
}

// -----------------------------------  EOF ---------------------------------------------------
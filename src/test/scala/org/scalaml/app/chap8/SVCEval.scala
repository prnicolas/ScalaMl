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
package org.scalaml.app.chap8

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.Fundamentals
import org.scalaml.supervised.svm.{SVMConfig, SVM, SVMExecution}
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.supervised.svm.formulation._
import org.scalaml.supervised.svm.kernel._
import Fundamentals._
import ScalaMl._
import SVMConfig._
import org.scalaml.util.Display
import org.apache.log4j.Logger
import scala.util.{Try, Success, Failure}
import org.scalaml.app.Eval


object SVCEval extends Eval {
	val name: String = "SVCEval"
	val maxExecutionTime: Int = 25000
	
	private val path = "resources/data/chap8/dividends2.csv"	
	private val C = 1.0
	private val GAMMA = 0.5
	private val EPS = 1e-3
	private val NFOLDS = 2
	
	private val logger = Logger.getLogger(name)

		/** <p>Execution of the scalatest for <b>SVC</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		Display.show(s"\n\n *****  test#${Eval.testCount} $name Binary Support Vector Classifier", logger)

		val extractor = relPriceChange :: 
						debtToEquity ::
						dividendCoverage ::
						cashPerShareToPrice ::
						epsTrend ::
						shortInterest :: 
						dividendTrend :: 
						List[Array[String] =>Double]()
	   
		Try {
			val xs = DataSource(path, true, false, 1) |> extractor
			val config = SVMConfig(new CSVCFormulation(C), new RbfKernel(GAMMA), SVMExecution(EPS, NFOLDS))
	  	  
			val features = XTSeries.transpose(xs.dropRight(1))
			val svc = SVM[Double](config, features, xs.last)
		     
			Display.show(s"${svc.toString}", logger)
			svc.accuracy match {
				case Some(acc) => Display.show(s"$name.run completed", logger)
				case None => Display.error(s"$name.run accuracy could not be computed", logger)
			}
		} 
		match {
			case Success(n) => n
			case Failure(e) => Display.error(s"$name.run Could not validate the training set", logger, e)
		}
	}
}


// --------------------------  EOF -----------------------------------------------
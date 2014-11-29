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
import org.scalaml.supervised.svm._
import org.scalaml.core.{XTSeries, Types}
import org.scalaml.supervised.svm.kernel.RbfKernel
import org.scalaml.supervised.svm.formulation.OneSVCFormulation
import Fundamentals._
import Types.ScalaMl._
import org.scalaml.util.Display
import org.apache.log4j.Logger
import scala.util.{Try, Success, Failure}
import org.scalaml.app.Eval

	
object SVCOutliersEval extends Eval {
	val name: String = "SVCOutliersEval"
	val maxExecutionTime: Int = 25000
	
	val path = "resources/data/chap8/dividends2.csv"	

	val NU = 0.2
	val GAMMA = 0.5
	val EPS = 1e-3
	val NFOLDS = 2
	
	private val logger = Logger.getLogger(name)
	
		/** <p>Execution of the scalatest for the one-class <b>SVC</b> for outliers.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		Display.show(s"\n\n *****  test#${Eval.testCount} $name Evaluation of One class Support Vector Classifier", logger)
		val extractor = relPriceChange :: 
						debtToEquity ::
						dividendCoverage ::
						cashPerShareToPrice ::
						epsTrend ::
						dividendTrend :: List[Array[String] =>Double]()
	   	   
		val filter = (x: Double) => if(x == 0) -1.0 else 1.0
		Try {
			val xs = DataSource(path, true, false, 1) |> extractor
 	
			val config = SVMConfig(new OneSVCFormulation(NU), new RbfKernel(GAMMA), SVMExecution(EPS, NFOLDS))
			val features = XTSeries.transpose(xs.dropRight(1))
			val svc = SVM[Double](config, features, xs.last.map( filter(_)) )
     
			Display.show(s"$name support vector machine model\n ${svc.toString}", logger)   
		        
			svc.accuracy match {
				case Some(acc) => Display.show(s"$name completed", logger)
				case None => Display.error(s"$name Could not validate the training set", logger)
			}
		} match {
			case Success(n) => n
			case Failure(e) => Display.error(s"$name.run failed to load source or train SVM", logger, e) 
		}
	}
}


// --------------------------  EOF -----------------------------------------------
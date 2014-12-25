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
package org.scalaml.app.chap8

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.Fundamentals
import org.scalaml.core.{XTSeries, Types}
import org.scalaml.supervised.svm.{SVM, SVMConfig, SVMExecution}
import org.scalaml.supervised.svm.kernel.RbfKernel
import org.scalaml.supervised.svm.formulation.OneSVCFormulation
import org.scalaml.util.DisplayUtils
import org.scalaml.app.Eval


		/**
		 * <p><b>Purpose:</b>Singleton to evaluate the on-class SVM for anomalies or outliers 
		 * detection.</p>
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning Chapter 8 Kernel models and support vector machines.
		 */
object SVCOutliersEval extends Eval {
	import scala.util.{Try, Success, Failure}
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
	
		/** <p>Execution of the scalatest for the one-class <b>SVC</b> for outliers.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Evaluation of One class Support Vector Classifier", logger)
		val extractor = relPriceChange :: 
						debtToEquity ::
						dividendCoverage ::
						cashPerShareToPrice ::
						epsTrend ::
						dividendTrend :: List[Array[String] =>Double]()
	   	   
		val filter = (x: Double) => if(x == 0) -1.0 else 1.0
		Try {
			val xs = DataSource(path, true, false, 1) |> extractor
 	
			val config = SVMConfig(new OneSVCFormulation(NU), new RbfKernel(GAMMA), 
					SVMExecution(EPS, NFOLDS))
			val features = XTSeries.transpose(xs.dropRight(1))
			val svc = SVM[Double](config, features, xs.last.map( filter(_)) )
     
			DisplayUtils.show(s"$name support vector machine model\n ${svc.toString}", logger)   
		        
			svc.accuracy
					.map(acc => DisplayUtils.show(s"$name completed", logger))
					.getOrElse(DisplayUtils.error(s"$name Could not validate the training set", logger))
		} match {
			case Success(n) => n
			case Failure(e) => failureHandler(e)
		}
	}
}


// --------------------------  EOF -----------------------------------------------
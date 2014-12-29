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

import org.scalaml.supervised.svm.{SVMConfig, SVM}
import org.scalaml.supervised.svm.formulation.CSVCFormulation
import org.scalaml.supervised.svm.kernel.RbfKernel
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.util.{FormatUtils, DisplayUtils}
import org.scalaml.app.Eval

		/**
		 * <p>Singleton to evaluate the impact of margin value on =
		 * on the accuracy of the classification of a binary support vector
		 * classifier using synthetic features. The synthetic values are generated
		 * using a combination of random generators. </p>
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning Chapter 8 Kernel models and support vector machines.
		 */
object SVCMarginEval extends Eval {
	import scala.util.{Random, Try, Success, Failure}
	import org.apache.log4j.Logger
	import XTSeries._, ScalaMl._
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "SVCMarginEval"
	
	private val GAMMA = 0.8
	val N = 100	
	private var status: Int = 0
    

		/** <p>Execution of the scalatest for evaluating margin in <b>SVC</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate<br>
		 * Main evaluation routine that consists of two steps:<br>
		 * Generation of synthetic features<br>
		 * Computation of the margin for a specific C penalty value</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {   
		DisplayUtils.show(s"$header Evaluation of impact of C penalty on margin", logger)
		val values = generate
		Try {
			Range(0, 50).foreach(i => evalMargin(values._1, values._2, i*0.1))
			status
		} match {
			case Success(status) => status
			case Failure(e) => failureHandler(e)
		}
	}

	private def generate: (DblMatrix, DblVector) = {
		val z  = Array.tabulate(N)(i =>{
			val ri = i*(1.0 + 0.2*Random.nextDouble)	
			Array[Double](i, ri)
		}) ++
		Array.tabulate(N)(i => Array[Double](i, i*Random.nextDouble))

		(z, Array.fill(N)(1) ++ Array.fill(N)(-1))
	}

	private def evalMargin(features: DblMatrix, lbl: DblVector, c: Double): Int = {
		val config = SVMConfig(new CSVCFormulation(c), new RbfKernel(GAMMA))
		val svc = SVM[Double](config, XTSeries[DblVector](features), lbl)
		
		DisplayUtils.show(s"\n$name Margin for SVC with\nC\tMargin", logger)
		svc.margin.map(_margin => { 
			val margin_str = FormatUtils.format(_margin, "", FormatUtils.ShortFormat)
			DisplayUtils.show(s"\n${c.floor}\t${margin_str}", logger)
		})
		.getOrElse(DisplayUtils.error(s"$name CSVC Formulation training failed", logger))
	}
}


// --------------------------- EOF --------------------------------------------------
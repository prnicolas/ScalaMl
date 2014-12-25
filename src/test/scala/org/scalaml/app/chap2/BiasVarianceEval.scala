/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.app.chap2

import org.scalaml.stats.{Stats, BiasVarianceEmulator}
import org.scalaml.core.Types.ScalaMl
import org.scalaml.util.{FormatUtils, DisplayUtils}
import org.scalaml.app.Eval

		/**
		 * <p><b>Purpose</b>Singleton to evaluate the bias-variance trade-off
		 * using synthetically generated data</p>
		 * 
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning  Chapter 2
		 */
object BiasVarianceEval extends Eval {
	import scala.util.{Try, Success, Failure, Random}
	import org.apache.log4j.Logger
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "BiasVarianceEval"
	
		/**
		 * <p>Execution of the scalatest for <p>Bias Variance </p> decomposition. 
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Evaluation of Bias Variance decomposition", logger)
		
		val testData = (x: Double) => 0.199*x*(1.02 + Math.sin(x*(0.05 + 0.01*(Random.nextDouble-0.5)))) 
				- 30.0*(Random.nextDouble-0.5)
	    
		val emul = (x: Double) => 0.2*x*(1.0 + Math.sin(x*0.05))
		val fEst = List[(Double=>Double, String)] (
				((x: Double) => 0.2*x, "y=x/5"),
				((x: Double) => 0.0003*x*x + 0.18*x, "y=3E-4x^2 + 0.18x"),
				((x: Double) => 0.201*x*(0.99 + Math.sin(x*0.05)), "y=x(1+sin(x/20))/5" )
		)
        
			// Uses jFreeChart to display the test data and the three models.
		DisplayUtils.show(s"$name plot for Bias Variance decomposition", logger)
		display(fEst, testData)
		
		Try {
			val modelFit = new BiasVarianceEmulator[Double](emul, 200)
			modelFit.fit(fEst.map( _._1))
							.map(bias => { 
				val result = FormatUtils.format(bias, "Variance", "bias", FormatUtils.ShortFormat)
				DisplayUtils.show(s"$name Result variance bias\n${result}", logger)
			}).getOrElse(DisplayUtils.error(s"$name variance bias model failed", logger))
		} 
		match {
			case Success(succeed) => DisplayUtils.show(s"$name Completed successfully", logger); 0
			case Failure(e) => failureHandler(e)
		}
	}
	
	private def display(estF: List[(Double =>Double, String)], f: Double =>Double): Unit = {
		import org.scalaml.plots.{LinePlot, BlackPlotTheme, LightPlotTheme}
		import ScalaMl._
		
		val data: List[(DblVector, String)] = (Array.tabulate(200)( f(_)), "f") :: 
				estF.foldLeft(List[(DblVector, String)]())((xs, g) => 
						(Array.tabulate(200)(y => g._1( y.toDouble)), g._2) :: xs)
		val labels = List[String]( name, "Bias-Variance Analysis", "x", "y")
		LinePlot.display(data, labels, new LightPlotTheme)
	}
}


// -------------------------------------  EOF -----------------------------------------
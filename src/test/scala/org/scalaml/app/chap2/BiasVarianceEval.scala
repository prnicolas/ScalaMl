/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 */
package org.scalaml.app.chap2

import org.scalaml.stats.{Stats, BiasVarianceEmulator}
import scala.util.Random
import org.scalaml.core.types.ScalaMl._
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display
import org.scalaml.app.Eval



	/**
	 * <p>Singleton to evaluate the Bias-Variance validation class.</p>
	 * 
	 * @author Patrick Nicolas
	 * @since February 1, 2014
	 * @note Scala for Machine Learning
	 */
object BiasVarianceEval extends Eval {
	val name: String = "BiasVarianceEval"
		
	import org.scalaml.plots.{LinePlot, BlackPlotTheme, LightPlotTheme}
		
	private val logger = Logger.getLogger(name)
	def run(args: Array[String]): Int = {
		Display.show("Evaluation of Bias Variance decomposition", logger)
		
		val testData = (x: Double) => 0.199*x*(1.02 + Math.sin(x*(0.05 + 0.01*(Random.nextDouble - 0.5)))) - 30.0*(Random.nextDouble-0.5)
	    
		val emul = (x: Double) => 0.2*x*(1.0 + Math.sin(x*0.05))
        val fEst = List[(Double=>Double, String)] (
	       ((x: Double)=> 0.2*x, "y=x/5"),
	       ((x: Double) => 0.0003*x*x + 0.18*x, "y=3E-4x^2 + 0.18x"),
		   ((x: Double) =>0.201*x*(0.99 + Math.sin(x*0.05)), "y=x(1+sin(x/20))/5" )
		)
        
			// Uses jFreeChart to display the test data and the three models.
		display(fEst, testData)
		
		Try {
		  val modelFit = new BiasVarianceEmulator[Double](emul, 200)
		  modelFit.fit(fEst.map( _._1)) match {
		    case Some(varBias) => Display.show(varBias, logger); true
		    case None => false
		  }
		} match {
			case Success(succeed) => Display.show("BiasVarianceEval completed ", logger); 0
			case Failure(e) => Display.error("BiasVarianceEval ", logger, e); -1
		}
	}
	
	private def display(estF: List[(Double =>Double, String)], f: Double =>Double): Unit = {
		val plot = new LinePlot((" ", "x", "y"), new LightPlotTheme)
		val data =  (Array.tabulate(200)( f(_)), "f") :: estF.foldLeft(List[(DblVector, String)]()) ((xs, g) => (Array.tabulate(200)(y => g._1( y.toDouble)), g._2) :: xs)  
		plot.display(data, 340, 280)
	}
}


// -------------------------------------  EOF -----------------------------------------
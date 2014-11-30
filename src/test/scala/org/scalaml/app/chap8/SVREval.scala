/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
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
import org.scalaml.trading.GoogleFinancials
import org.scalaml.supervised.svm.kernel.RbfKernel
import org.scalaml.supervised.svm.formulation.SVRFormulation
import org.scalaml.supervised.svm.{SVMConfig, SVM}
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl
import GoogleFinancials._
import ScalaMl._
import org.scalaml.supervised.regression.linear.SingleLinearRegression
import org.apache.log4j.Logger
import org.scalaml.util.Display
import scala.util.{Try, Success, Failure}
import org.scalaml.app.Eval


object SVREval extends Eval {
	val name: String = "SVREval"
	val maxExecutionTime: Int = 25000
	
	private val path = "resources/data/chap8/SPY.csv"
	private val C = 12
	private val GAMMA = 0.3
	private val EPS = 1e-3
	private val EPSILON = 2.5
   
	private val logger = Logger.getLogger(name)

		/** <p>Execution of the scalatest for evaluating the support vector regression.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		Display.show(s"\n\n *****  test#${Eval.testCount} $name Support Vector Regression", logger)
		Try {
			val price = DataSource(path, false, true, 1) |> close
			Display.show(ScalaMl.toString(price.toArray, "", true), logger)
			val priceIdx = price.zipWithIndex
								.map( x => (x._2.toDouble, x._1.toDouble))
	      
			val linRg = SingleLinearRegression(priceIdx)
	  	  	 
			val config = SVMConfig(new SVRFormulation(C, EPSILON), new RbfKernel(GAMMA))
		//		val config = SVMConfig(new SVRFormulation(C, EPSILON), LinearKernel)
			val labels = price.toArray
			val features = XTSeries[DblVector](Array.tabulate(labels.size)(Array[Double](_))) 
			val svr = SVM[Double](config, features, labels)
			Display.show(s"$name support vector machine model\n${svr.toString}", logger)   
          
			display("Support Vector vs. Linear Regression", 
					collect(svr, linRg, price).toList,
					List[String]("Support vector regression", "Linear regression", "Stock Price"))
					
			Display.show(s"$name.run completed", logger)
		} 
		match {
			case Success(n) => n
			case Failure(e) => Display.error(s"$name.run failed to load source or train SVM", logger, e)
		}
	}
   
	
	import SingleLinearRegression._
	private def collect(svr: SVM[Double], lin: SingleLinearRegression[Double], price: DblVector): Array[XYTSeries] = {
		import scala.collection.mutable.ArrayBuffer

		val collector = Array.fill(3)(new ArrayBuffer[XY])
		Range(1, price.size-80).foldLeft(collector)( (xs, n) => {
			xs(0).append((n, (svr |> n.toDouble)))
			xs(1).append((n, (lin |> n)))
			xs(2).append((n, price(n)))
			xs		  
		}).map( _.toArray)
	}
   
	private def display(label: String, xs: List[XYTSeries], lbls: List[String]): Unit = {
		import org.scalaml.plots.{ScatterPlot, LightPlotTheme}
		require(xs != null && xs.size > 0, s"$name Cannot display an undefined time series")
       
		val plotter = new ScatterPlot(("SVR SPY prices", label, "SPY"), new LightPlotTheme)
		plotter.display(xs, lbls, 250, 340)
	}
}

// --------------------------  EOF -----------------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95d
 */
package org.scalaml.app.chap8

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.YahooFinancials
import org.scalaml.supervised.svm._
import org.scalaml.core.{XTSeries, types}
import YahooFinancials._
import types.ScalaMl._
import org.scalaml.supervised.regression.linear.SingleLinearRegression
import org.apache.log4j.Logger
import org.scalaml.util.Display
import scala.util.{Try, Success, Failure}
import org.scalaml.app.Eval


object SVREval extends Eval {
   val name: String = "SVREval"
   final val path = "resources/data/chap8/SPY.csv"
   final val C = 1
   final val GAMMA = 0.8
   final val EPS = 1e-1
   final val EPSILON = 0.1
   
   private val logger = Logger.getLogger(name)
   
   def run(args: Array[String]): Int = {
	   Display.show("Evaluation of Support Vector Regression", logger)
       Try {
	      val price = DataSource(path, false, true, 1) |> adjClose	
	  	  val priceIdx = price.zipWithIndex
	  	  	                  .map( x => (x._1.toDouble, x._2.toDouble))
	      val linRg = SingleLinearRegression(priceIdx)  	  	
	  	  	 
	  	  val config = SVMConfig(new SVRFormulation(C, EPSILON), RbfKernel(GAMMA))
	  	  val labels = price.toArray
          val features = XTSeries[DblVector](Array.tabulate(labels.size)(Array[Double](_))) 
          val svr = SVM[Double](config, features, labels)
          Display.show(s"SVREval.run: ${svr.toString}", logger)   
          
          display("Support Vector vs. Linear Regression", 
            		  collect(svr, linRg, price).toList,
            		  List[String]("SVR", "Linear regression", "Stock Price"))
          Display.show("SVREval.run completed", logger)
	    } match {
	    	case Success(n) => n
	    	case Failure(e) => Display.error("SVREval.run", logger, e)
	    }
   }
   
   import SingleLinearRegression._
   private def collect(svr: SVM[Double], lin: SingleLinearRegression[Double], price: DblVector): Array[XYTSeries] = {
  	   import scala.collection.mutable.ArrayBuffer

  	   val collector = Array.fill(3)(new ArrayBuffer[XY])
  	   Range(1, price.size-2).foldLeft(collector)( (xs, n) => {
  	 	  xs(0).append((n, (svr |> n.toDouble)))
  	 	  xs(1).append((n, (lin |> n)))
  	 	  xs(2).append((n, price(n)))
  	 	  xs		  
  	   }).map( _.toArray)
   }
   
   private def display(label: String, xs: List[XYTSeries], lbls: List[String]): Unit = {
  	  import org.scalaml.plots.{ScatterPlot, LightPlotTheme}
       require(xs != null && xs.size > 0, "Cannot display an undefined time series")
       
       val plotter = new ScatterPlot(("Training set", label, "SPY"), new LightPlotTheme)
       plotter.display(xs, lbls, 250, 340)
	}
}

object SVREvalApp extends App {
	SVREval.run(Array.empty)
}

// --------------------------  EOF -----------------------------------------------
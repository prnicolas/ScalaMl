/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.app.chap8

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.YahooFinancials
import org.scalaml.supervised.svm._
import org.scalaml.core.{XTSeries, Types}
import YahooFinancials._
import Types.ScalaMl._
import org.scalaml.supervised.regression.linear.SingleLinearRegression


object SVREval {

   final val path = "resources/data/chap8/SPY.csv"
   final val C = 1
   final val GAMMA = 0.8
   final val EPS = 1e-1
   final val EPSILON = 0.1
   type SVM_Double = SVM[Double]
   
   def run: Unit = {
	   Console.println("Evaluation of Support Vector Regression")

	   DataSource(path, false, true, 1) |> adjClose match {	   
	  	  case Some(price) => {	
	  	  	 val xy = price.zipWithIndex.map( x => (x._1.toDouble, x._2.toDouble))
	  	  	 val lin = SingleLinearRegression(xy)  	  	
	  	  	 
	  	     val config = SVMConfig(SVRFormulation(C, EPSILON), RbfKernel(GAMMA))
	  	     val labels = price.toArray
             val features = XTSeries[DblVector](Array.tabulate(labels.size)(Array[Double](_))) 
             val svr = SVM[Double](config, features, labels)
             
             display("Support Vector vs. Linear Regression", 
            		  collect(svr, lin, price).toList,
            		  List[String]("SVR", "Linear regression", "Stock Price"))
	  	  }
	  	  case None => { }
	   }
   }
   
   private def collect(svr: SVM_Double, lin: SingleLinearRegression[Double], price: DblVector): Array[XYTSeries] = {
  	   import scala.collection.mutable.ArrayBuffer

  	   Range(1, price.size-2).foldLeft(Array.fill(3)(new ArrayBuffer[XY]))( (xs, n) => {
  	 	  xs(0).append((n, (svr |> n.toDouble).get))
  	 	  xs(1).append((n, (lin |> n).get))
  	 	  xs(2).append((n, price(n)))
  	 	  xs		  
  	   }).map( _.toArray)
   }
   
   private def display(label: String, xs: List[XYTSeries], lbls: List[String]): Unit = {
  	  import org.scalaml.plots.{ScatterPlot, LightPlotTheme}
  	 
       require(xs != null && xs.size > 0, "Cannot display an undefined time series")
       
       val plotter = new ScatterPlot(("Training set", label, "SPY"), new LightPlotTheme)
   //    val xxs = Array[XY]((0, 1), (3,7), (11,5))  :: Array[XY]((3, 1), (1,5), (12,8)) ::  Array[XY]((8, 1), (4,5), (2,10)) :: List[XYTSeries]()
       plotter.display(xs, lbls, 250, 340)
	}
}

// --------------------------  EOF -----------------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap8

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.PriceVolume
import org.scalaml.supervised.svm._
import org.scalaml.core.{XTSeries, Types}
import PriceVolume._
import Types.ScalaMl._
import org.scalaml.supervised.regression.linear.SingleLinearRegression




		/**
		 * <p>Singleton to evaluate support vector regression using
		 * the S&P 500 ETF, SPY. The prediction is compared to the original
		 * labeled observations and the output of the linear regression.</p>
		 * 
		 * @author Patrick Nicolas
		 * @date April 30, 2014
		 */
object SVREval {

   final val path = "resources/data/chap8/SPY.csv"
   final val C = 1
   final val GAMMA = 0.8
   final val EPS = 1e-1
   final val EPSILON = 0.1

   
   def run: Unit = {
	   Console.println("Evaluation of Support Vector Regression")

	   
	   DataSource(path, false, true, 1) |> adjClose match {	   
	  	  case Some(price) => {	
	  	  	 val lin = SingleLinearRegression(price)  	  	
	  	  	 
	  	     val config = SVMConfig(SVRFormulation(C, EPSILON), RbfKernel(GAMMA))
	  	     val labels = price.toArray
             val features = XTSeries[DblVector](Array.tabulate(labels.size)(Array[Double](_))) 
             val svr = new SVM[Double](config, features, labels)
             
             display("Support Vector vs. Linear Regression", 
            		  collect(svr, lin, price).toList,
            		  List[String]("SVR", "Linear regression", "Stock Price"))
	  	  }
	  	  case None => { }
	   }
   }
   
   
   private def collect(svr: SVM[Double], lin: SingleLinearRegression[Double], price: DblVector): Array[XYTSeries] = {
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
   /*
   def validate(features: DblMatrix, svr: SVC[Double]): Unit = {
       features.zipWithIndex.toArray
  		         .foreach( xti => {
  		            val predicted = svr |> xti._1
  		            println("predicted: " + predicted.get) })              
  }
   
   private def predict(stock: DblVector, svr: SVC[Double]) {
  	 XTSeries.normalize(stock) match {
	  	case Some(normalizedStock) =>   
	  	   svr |> normalizedStock  match {
	  		 case Some(prob) => println("prediction: " + prob)
	  		 case None => println("Could not validate the training set")
	  	   }
	  	case None => println("Failed to normalize new feature")
	 }
   }
   * 
   */
}


object SVREvalApp extends App {
	SVREval.run
	/*
	val x1 = Array[Double](2,0, 5,0, 6,0, 7.0)
    val x2 = Array[Double](67,0, 15,0, 36,0, 27.0)
    val x3 = Array[Double](167,0, 115,0, 136,0, 127.0)
    
    val xs1 = x1 :: x2 :: x3 :: List[DblVector]()
    println("Original Matrix")
    xs1.foreach( v => println( v.foldLeft(new StringBuilder)((s, x) => s.append(x).append(","))))
    
    val result: Array[DblVector] = XTSeries.transpose(xs1)
    
        println("\nTranspose Matrix")
    result.foreach( v => println( v.foldLeft(new StringBuilder)((s, x) => s.append(x).append(","))))
    * 
    */
}

// --------------------------  EOF -----------------------------------------------
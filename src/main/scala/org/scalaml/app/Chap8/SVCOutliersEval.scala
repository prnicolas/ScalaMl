/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap8

import org.scalaml.workflow.data.DataSource
import org.scalaml.trading.DividendPayout
import org.scalaml.supervised.svm._
import org.scalaml.core.{XTSeries, Types}


import DividendPayout._
import Types.ScalaMl._
object SVCOutliersEval {
   final val path = "resources/data/chap8/dividends2.csv"	
   final val NU = 0.2
   final val GAMMA = 0.5
   final val EPS = 1e-3
   final val NFOLDS = 2
	
	def run: Unit = {
	   Console.println("Evaluation of One class Support Vector Classifier")
	   val extractor = relPriceChange :: 
	                   debtToEquity ::
	                   dividendCoverage ::
	                   cashPerShareToPrice ::
	              //     epsTrend ::
	                   dividendTrend :: List[Array[String] =>Double]()
	   	   
	   DataSource(path, true, false, 1) |> extractor match {
	  	 case Some(xs) => {	  	
	  		 val config = SVMConfig(OneSVCFormulation(NU), RbfKernel(GAMMA), SVMExecution(EPS, NFOLDS))
	  		 val features = XTSeries.transpose(xs.take(xs.size-1))
		     val svc = SVM[Double](config, features, xs.last.map(x => if( x == 0.0) -1.0 else 1.0))
          
		     svc.accuracy match {
	  			 case Some(acc) => println("Accuracy: " + acc)
	  			 case None => println("Could not validate the training set")
	  		 }
	  	 }
	  	 
	  	 case None => println("Dividend data extration failed ")
	   }
	}
}


object SVCOutliersEvalApp extends App {
	SVCOutliersEval.run
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
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



		/**
		 * <p>Singleton to evaluate the binary support vector classifier. The
		 * purpose of the test is to extract the features set (or subset) and
		 * the 2 classes among the three labels values {-1, 0, +1} that provide
		 * the best accuracy.</p>
		 * 
		 * @author Patrick Nicolas
		 * @date April 30, 2014
		 */
import DividendPayout._
import Types.ScalaMl._
object SVCEval {
   final val path = "resources/data/chap8/dividends.csv"	
   final val C = 1.0
   final val GAMMA = 0.5
   final val EPS = 1e-3
   final val NFOLDS = 2
	
   def run: Unit = {
	   Console.println("Evaluation of Binary Support Vector Classifier")
	   val extractor = relPriceChange :: 
	                   debtToEquity ::
	                   dividendCoverage ::
	                   cashPerShareToPrice ::
	                   epsTrend ::
	                   shortInterest :: 
	                   dividendTrend :: List[Array[String] =>Double]()
	   
	   val filter = Some((s: Array[String]) => s(DIVIDEND_TREND.id) != "-1")
	   
	   DataSource(path, true, false, 1, filter) |> extractor match {
	  	 case Some(xs) => {	  	
	  		 val config = SVMConfig(CSVCFormulation(C), RbfKernel(GAMMA), SVMExecution(EPS, NFOLDS))
	  		 val features = XTSeries.transpose(xs.take(xs.size-1))
		     val svc = SVM[Double](config, features, xs.last)
		     
		     svc.accuracy match {
	  			 case Some(acc) => println("Accuracy: " + acc)
	  			 case None => println("Could not validate the training set")
	  		 }
	  	 }
	  	 case None => println("Dividend data extration failed ")
	   }
	}
}


// --------------------------  EOF -----------------------------------------------
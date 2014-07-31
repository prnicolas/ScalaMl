/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap4

import org.scalaml.core.{Types, XTSeries}
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.{DataSource, DataSink}
import YahooFinancials._
import Types.ScalaMl._




object EMEval extends UnsupervisedLearningEval {
    import org.scalaml.unsupervised.em.MultivariateEM
    import org.scalaml.filtering.SimpleMovingAverage
    import SimpleMovingAverage._
    import MultivariateEM._
    	
   override def run(args: Array[String]): Unit = {
	 require(args != null && args.length != 2, "Cannot evaluate EM with undefined arguments")
     println("Evaluation of EM clustering")
     
	 val K = args(0).toInt
	 val samplingRate = args(1).toInt
     val period = 8
     val smAve = SimpleMovingAverage[Double](period)
        
     		// extracts the observations from a set of csv files.
     val obs = symbols.map(sym => {
        DataSource(sym, path, true) |> extractor match {
          case Some(xs) => {
              val values: XTSeries[Double] = (XTSeries.|>(xs)).head  // force a data type conversion (implicit)

              smAve |> values match {
                case Some(filtered) => {
                   filtered.zipWithIndex
                           .drop(period+1)
                           .toArray
                           .filter( _._2 % samplingRate == 0).map( _._1)
                }
                case None =>null
              }
          }
          case None => null
        }
     })
     
     	// If all the observations are valid
     if( obs.find( _ == null) != None) {
    	 
        MultivariateEM[Double](K) |> XTSeries[DblVector](obs) match {
		  case Some(components) => components.foreach( x => {
		      println(x._1 + " means: ")
		      x._2.foreach( println )
		      println(" stddev: ")
		      x._3.foreach( println )
		  })
		  case None => Console.println("Error")
	   }
	 }
     else 
    	 println("Some observations are corrupted")
   }
}




// -----------------------------------  EOF ---------------------------------------------------
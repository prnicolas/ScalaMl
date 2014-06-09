/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap3



import org.scalaml.core.{Types, XTSeries}
import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.PriceVolume
import PriceVolume._


trait FilteringEval {
  def run(args: Array[String]): Unit
}



object Chap3 extends App {
   try {
	  MovingAveragesEval.run(Array[String]("BAC", "10"))
	  DFTEval.run(Array[String]("BAC"))
	  
	  implicit def double2String(x: Double): String = x.toString
	  (new DKalmanEval).run(Array[String]("BAC"))
   }
   catch {
  	  case e: IllegalArgumentException => Console.println("Filter evaluation failed " + e.toString)
  	  case e: RuntimeException => Console.println("Filter evaluation failed " + e.toString)
   }
 
}


// --------------------------------------  EOF -------------------------------
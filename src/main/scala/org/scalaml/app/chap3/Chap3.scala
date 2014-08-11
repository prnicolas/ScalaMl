/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap3



import org.scalaml.core.{Types, XTSeries}
import org.scalaml.workflow.data.{DataSource, DataSink}
import org.scalaml.trading.YahooFinancials
import YahooFinancials._


trait FilteringEval {
  def run(args: Array[String]): Unit
}



object Chap3 extends App {
	private def runAll = {
      MovingAveragesEval.run(Array[String]("BAC", "10"))
	  DFTEval.run(Array[String]("BAC"))
	  
	  implicit def double2String(x: Double): String = x.toString
	  (new DKalmanEval).run(Array[String]("BAC"))
	}
		
	final val cmdDescriptor: String = {
		new StringBuilder("Command line: Chap 3 arg\n")
		   .append(" mvaverage: Evaluation of moving average\n")
		   .append(" fourier:  Evaluation of Discrete Fourier\n")
		   .append(" kalman: Evaluation of Kalman filter\n")
		   .append(" all: All evaluation").toString
	}
	
	try {
	   if( args == null || args.length == 0) "?" else args(0) match {
			case "?" => println(cmdDescriptor)
			case "maverage" => MovingAveragesEval.run(Array[String]("BAC", "10"))
			case "fourier" =>   DFTEval.run(Array[String]("BAC"))
			case "kalman" => {
			   implicit def double2String(x: Double): String = x.toString
		       (new DKalmanEval).run(Array[String]("BAC"))
			}
			case "all" => runAll
			case _ =>  println(cmdDescriptor)
	   }
	}
	catch {
	  case e: IllegalArgumentException => println("Filtering evaluation failed " + e.toString)
	  case e: RuntimeException =>  println("Filtering evaluation failed " + e.toString)
	}
}


// --------------------------------------  EOF -------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.app.chap3

import org.scalaml.app.ScalaMlApp


trait FilteringEval {
  def run(args: Array[String]): Unit
}



object Chap3 extends App with ScalaMlApp {
	
	private def runAll = {
      MovingAveragesEval.run(Array[String]("BAC", "10"))
      DFTEval.run(null)
	  DFTEval.run(Array[String]("BAC"))
	  
	  implicit def double2String(x: Double): String = x.toString
	  (new DKalmanEval).run(Array[String]("BAC"))
	}
		
	final val cmdDescriptor: String = {
		new StringBuilder("Command line: Chap 3 arg\n")
		   .append(" mvaverage: Evaluation of moving average\n")
		   .append(" fourier:  Evaluation of Discrete Fourier with financial data\n")
		   .append(" kalman: Evaluation of Kalman filter\n")
		   .append(" all: All evaluation").toString
	}
	
	
   override protected def execute(args: Array[String]): String = {
	   if( args == null || args.length == 0) "?" else args(0) match {
			case "?" => cmdDescriptor
			case "maverage" => MovingAveragesEval.run(Array[String]("BAC", "10")); args(0)
			case "fourier" =>   {
				DFTEval.run(null)
				DFTEval.run(Array[String]("BAC"))
				args(0)
			}
			case "kalman" => {
			   implicit def double2String(x: Double): String = x.toString
		       (new DKalmanEval).run(Array[String]("BAC"))
		       args(0)
			}
			case "all" => runAll; args(0)
			case _ =>  cmdDescriptor
	   }
	}
	
	process(args)
}


// --------------------------------------  EOF -------------------------------
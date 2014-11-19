/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 */
package org.scalaml.app.chap3


import org.scalaml.app.ScalaMlTest
import scala.language.implicitConversions
import org.scalaml.app.Eval

trait FilteringEval extends Eval {
  def run(args: Array[String]): Int
}


final class Chap3 extends ScalaMlTest  {  
	val chapter: String = "Chap 3"
		
    implicit def double2String(x: Double): String = x.toString
	test(s"$chapter Moving averages evaluation") {
		evaluate(MovingAveragesEval, Array[String]("BAC", "10")) 
	}
	
	test(s"$chapter Discrete Fourier Series synthetic evaluation") {
	   evaluate(DFTEval)
	}
	
	test(s"$chapter Discrete Fourier Series evaluation on stock") {
	   evaluate(DFTEval, Array[String]("BAC"))
	}
	
	test(s"$chapter Kalman filter evaluation") {
	   evaluate(new DKalmanEval, Array[String]("BAC"))
	}
}


// --------------------------------------  EOF -------------------------------
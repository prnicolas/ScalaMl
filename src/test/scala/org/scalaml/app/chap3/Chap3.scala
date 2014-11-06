/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95
 */
package org.scalaml.app.chap3


import org.scalatest.FunSuite
import scala.language.implicitConversions

trait FilteringEval {
  def run(args: Array[String]): Int
}


final class Chap3 extends FunSuite {  
    implicit def double2String(x: Double): String = x.toString
	test("Moving averages evaluation") {
	  assert(MovingAveragesEval.run(Array[String]("BAC", "10")) != -1)
	}
	
	test("Discrete Fourier Series synthetic evaluation") {
	  assert(DFTEval.run(null) != -1)
	}
	
	test("Discrete Fourier Series evaluation on stock") {
	  assert(DFTEval.run(Array[String]("BAC")) != -1)
	}
	
	test("Kalman filter evaluation") {
	   (new DKalmanEval).run(Array[String]("BAC"))
	}
}


// --------------------------------------  EOF -------------------------------
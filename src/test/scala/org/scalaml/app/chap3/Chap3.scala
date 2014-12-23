/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.app.chap3


import org.scalaml.app.ScalaMlTest
import scala.language.implicitConversions
import org.scalaml.app.Eval


		/**
		 * Trait to evaluate the filtering techniques presented in Chapter 3
		 * @see org.scalaml.app.Eval
		 * @author Patrick Nicolas
		 * @since May 28, 2014
		 * @note Scala for Machine Learning Chapter 3 Data pre-processing
		 */
trait FilteringEval extends Eval {
	
		/** 
		 * <p>Execution of the scalatest for filtering technique.</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int
}


		/**
		 * <p>Test driver for the techniques described in the Chapter 3 Data pre-processing<br>
		 * <ul>
		 * 	 <li>Moving averages</li>
		 *   <li>Discrete Fourier Series with Synthetic time series</li>
		 *   <li>Discrete Fourier Series for financial analysis</li>
		 *   <li>Kalman filter for financial analysis</li>
		 * </ul></p>
		 * @see org.scalaml.app.ScalaMlTest
		 * @author Patrick Nicolas
		 * @since May 28, 2014
		 * @note Scala for Machine Learning Chapter 3 Data pre-processing
		 */
final class Chap3 extends ScalaMlTest  {  
		/**
		 * Name of the chapter the tests are related to
		 */
	val chapter: String = "Chap 3"
			/**
		 * Maximum duration allowed for the execution of the evaluation
		 */
	val maxExecutionTime: Int = 10
		
	implicit def double2String(x: Double): String = x.toString
	test(s"$chapter Moving averages") {
		evaluate(MovingAveragesEval, Array[String]("BAC", "10")) 
	}
	
	test(s"$chapter Discrete Fourier Series with Synthetic time series") {
		evaluate(DFTEval)
	}
	
	test(s"$chapter Discrete Fourier Series for financial analysis") {
		evaluate(DFTEval, Array[String]("BAC"))
	}
	
	test(s"$chapter Kalman filter for financial analysis") {
	   evaluate(DKalmanEval, Array[String]("BAC"))
	}
}


// --------------------------------------  EOF -------------------------------
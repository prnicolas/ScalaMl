/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99
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
		 * Execution of the scalatest for filtering technique.
		 * @param args array of arguments used in the test
		 */
	override protected def run(args: Array[String]): Int
}


		/**
		 * Test driver for the techniques described in the Chapter 3 Data pre-processing
		 * 
		 *  - Moving averages
		 *  
		 *  - Discrete Fourier Series with Synthetic time series
		 *  
		 *  - Discrete Fourier Series for financial analysis
		 *  
		 *  - Kalman filter for financial analysis
		 * 
		 * @see org.scalaml.app.ScalaMlTest
		 * @author Patrick Nicolas
		 * @since May 28, 2014
		 * @see Scala for Machine Learning Chapter 3 ''Data pre-processing''
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
	
	test(s"$chapter Time series methods") {
		evaluate(XTSeriesEval)
	}
	
	test(s"$chapter Moving averages test") {
		evaluate(MovingAveragesEval2) 
	}
		
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
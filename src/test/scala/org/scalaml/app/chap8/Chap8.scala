/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.app.chap8

import org.scalaml.app.ScalaMlApp



	/**
	 * <p>Singleton to run the test cases presented in chapter 8
	 * 
	 *  @author Patrick Nicolas
	 *  @since May 7. 2014
	 *  @note Scala for Machine Learning
	 */
object Chap8 extends App with ScalaMlApp {
   private def runAll = {
	 SVCMarginEval.run
	 SVCKernelEval.run
	 SVCEval.run
	 SVCOutliersEval.run
  }
	
  final val cmdDescriptor: String = {
	new StringBuilder("Command line: Chap 8 [args]\n")
		  .append(" margin: C penalty factor and Margin test case\n")
		  .append(" kernel: evaluation of kernel functions test case\n")
		  .append(" svc: evaluation binary support vector classifier test case\n")
		  .append(" outliers: evaluation one class support vector classifier for anomalies detection test case\n")
		  .append(" all: All test cases").toString
  }
  
  override protected def execute(args: Array[String]): String = {
    if(args == null || args.length == 0) "?" else args(0) match {
  	   case "?" => cmdDescriptor
  	   case "margin" => SVCMarginEval.run; args(0)
  	   case "kernel" => SVCKernelEval.run; args(0)
  	   case "svc" => SVCEval.run; args(0)
  	   case "outliers" => SVCOutliersEval.run; args(0)
  	   case "all" => runAll; args(0)
  	   case _ => cmdDescriptor
    }
  }
  process(args)
}
	

// --------------------------- EOF --------------------------------------------------
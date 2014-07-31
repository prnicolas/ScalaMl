/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap8



	/**
	 * <p>Singleton to run the test cases presented in chapter 8
	 * 
	 *  @author Patrick Nicolas
	 *  @date May 7. 2014
	 *  @project Scala for Machine Learning
	 */
object Chap8 extends App {
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
  
  val argument = if(args == null || args.length == 0) "?" else args(0)

  argument match {
  	 case "?" => println(cmdDescriptor)
  	 case "margin" => SVCMarginEval.run
  	 case "kernel" => SVCKernelEval.run
  	 case "svc" => SVCEval.run
  	 case "outliers" => SVCOutliersEval.run
  	 case "all" => runAll
  	 case _ =>  println(cmdDescriptor)
  }
}
	

// --------------------------- EOF --------------------------------------------------
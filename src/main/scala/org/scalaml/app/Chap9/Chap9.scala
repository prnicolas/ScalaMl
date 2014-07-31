/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap9


object Chap9 extends App {
	MLPConfigEval.run(Array[String]("eta"))
}

/*
object Chap9 extends App {
  private def runAll = {
	 MLPConfigEval.run(Array[String]("eta"))
	 MLPConfigEval.run(Array[String]("alpha"))
	 BinaryMLPEval.run
  }
	
  final val cmdDescriptor: String = {
	new StringBuilder("Command line: Chap 9 [args]\n")
		  .append(" eta: evaluation of learning rate eta test case\n")
		  .append(" alpha: evaluation of momentum factor, alpha test case\n")
		  .append(" binMlp : evaluation of the binary Multi layer Perceptron classifier test case\n")
		  .append(" all: All test cases").toString
  }
  
  val argument = if(args == null || args.length == 0) "?" else args(0)

  argument match {
  	 case "?" => println(cmdDescriptor)
  	 case "eta" =>  MLPConfigEval.run(Array[String]("eta"))
  	 case "alpha" => MLPConfigEval.run(Array[String]("alpha"))
  	 case "binMlp" =>  BinaryMLPEval.run
  	 case "all" => runAll
  	 case _ =>  println(cmdDescriptor)
  }
}
* 
*/


// ---------------------------   EOF ------------------------------------------------
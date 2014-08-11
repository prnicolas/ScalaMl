/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap9


object Chap9 extends App {
  private def runAll = {
	 MLPConfigEval.run(Array[String]("eta"))
	 MLPConfigEval.run(Array[String]("alpha"))
	 MLPValidation.run(null)
	 BinaryMLPEval.run
	 MLPEval.run(null)
  }
  
  final val cmdDescriptor: String = {
	new StringBuilder("Command line: Chap 9 arg\n")
		 .append(" eta: evaluation of learning rate on MLP execution\n")
		   .append(" alpha: evaluation of momentum factor on MLP execution\n")
		     .append(" validation: simple validation of MLP model\n")
		       .append(" binary: evaluation binary MLP classifier\n")
		         .append(" accuracy: evaluation of accuracy of MLP classifier\n")
		           .append(" all: all MLP test cases").toString
		        
  }
  
  try {
	 if( args == null || args.length == 0) "?" else args(0) match {
	  	case "?" => println(cmdDescriptor)
	  	case "eta" =>  MLPConfigEval.run(Array[String]("eta"))
	    case "alpha" => MLPConfigEval.run(Array[String]("alpha"))
	  	case "validation" => MLPValidation.run(null)
	  	case "binary" =>  BinaryMLPEval.run
	  	case "accuracy" => MLPEval.run(null)
	  	case "all" => runAll
	  	case _ =>  println(cmdDescriptor)
	 }
  }
  catch {
     case e: IllegalArgumentException => println("Evaluation MLP failed: " + e.toString)
     case e: RuntimeException => println("Evaluation MLP failed: " + e.toString)
  }
}


// ---------------------------   EOF ------------------------------------------------
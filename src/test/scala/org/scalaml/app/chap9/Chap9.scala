/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.app.chap9

import org.scalaml.app.ScalaMlApp


object Chap9 extends App with ScalaMlApp {
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
  
  
  override protected def execute(args: Array[String]): String = {
	 if( args == null || args.length == 0) "?" else args(0) match {
	  	case "?" => cmdDescriptor
	  	case "eta" =>  MLPConfigEval.run(Array[String]("eta")); args(0)
	    case "alpha" => MLPConfigEval.run(Array[String]("alpha")); args(0)
	  	case "validation" => MLPValidation.run(null); args(0)
	  	case "binary" =>  BinaryMLPEval.run; args(0)
	  	case "accuracy" => MLPEval.run(null); args(0)
	  	case "all" => runAll; args(0)
	  	case _ => cmdDescriptor
	 }
  }

  process(args)
}


// ---------------------------   EOF ------------------------------------------------
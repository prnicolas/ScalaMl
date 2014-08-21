/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap10

import org.scalaml.app.ScalaMlApp



object Chap10 extends App with ScalaMlApp  {
	private def runAll = {
  	  GAEval.run
   }
	
   final val cmdDescriptor: String = {
	  new StringBuilder("Command line: Chap 10 arg\n")
		   .append(" gaeval: Evaluation of genetic algorithm\n")
		   .append(" all: All evaluation").toString
	}
	
  override protected def execute(args: Array[String]): String = {
	 if( args == null || args.length == 0) "?" else args(0) match {
		case "?" => cmdDescriptor
		case "ga" => GAEval.run; args(0)
		case "all" => runAll; args(0)
		case _ => cmdDescriptor
	   }	
   }
  
   process(args)
}


// ----------------------------------  EOF ----------------------------------------------------------------
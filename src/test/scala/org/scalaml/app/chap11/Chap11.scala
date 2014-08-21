/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap11

import org.scalaml.app.ScalaMlApp




object Chap11 extends App with ScalaMlApp {  
   private def runAll = {
  	  QLearningEval.run
   }
	
   final val cmdDescriptor: String = {
	  new StringBuilder("Command line: Chap 11 arg\n")
		   .append(" qlearning: Evaluation of Q-Learning algorithm\n")
		   .append(" all: All evaluation").toString
	}
	
  override protected def execute(args: Array[String]): String = {
	 if( args == null || args.length == 0) "?" else args(0) match {
		case "?" => cmdDescriptor
		case "qlearning" => QLearningEval; args(0)
		case "all" => runAll; args(0)
		case _ => cmdDescriptor
	   }	
   }

   process(args)
}

// ------------------------------------  EOF ----------------------------------
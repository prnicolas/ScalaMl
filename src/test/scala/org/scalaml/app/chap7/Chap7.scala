/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap7

import org.scalaml.app.ScalaMlApp



	/**
	 * Singleton to execute the test cases presented in Chapter7
	 * 
	 * @author Patrick Nicolas
	 * @since March 25, 2014
	 * @note Scala for Machine Learning
	 */
object Chap7 extends App with ScalaMlApp {
		 
   private def runAll = {
	 HMMEval.run
	 CrfEval.run
  }
	
  final val cmdDescriptor: String = {
	new StringBuilder("Command line: Chap 7 arg\n")
		  .append(" hmm: Hidden Markov Model test case\n")
		     .append(" crf:  Conditional random fields test case\n")
		        .append(" all: All test cases").toString
  }
  
  override protected def execute(args: Array[String]): String = {
	  if( args == null || args.length == 0) "?" else args(0) match {
	  	 case "?" => cmdDescriptor
	  	 case "hmm" => HMMEval.run; args(0)
	  	 case "crf" => CrfEval.run; args(0)
	  	 case "all" => runAll; args(0)
	  	 case _ => cmdDescriptor
	  }
  }

  process(args)
}

// --------------------------------  EOF -------------------------------
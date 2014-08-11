/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap7

import org.scalaml.util.Matrix
import org.scalaml.supervised.hmm.{HMM, HMMForm, HMMLambda}
import org.scalaml.supervised.crf.{CrfConfig,  CrfSeqDelimiter, Crf}
import java.io.IOException



	/**
	 * Singleton to execute the test cases presented in Chapter7
	 * 
	 * @author Patrick Nicolas
	 * @date March 25, 2014
	 * @project Scala for Machine Learning
	 */
object Chap7 extends App {
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
  
  try {
	  if( args == null || args.length == 0) "?" else args(0) match {
	  	 case "?" => println(cmdDescriptor)
	  	 case "hmm" => HMMEval.run
	  	 case "crf" => CrfEval.run
	  	 case "all" => runAll
	  	 case _ =>  println(cmdDescriptor)
	  }
  }
  catch {
     case e: IllegalArgumentException => println("HMM or CRF evaluation failed " + e.toString)
     case e: RuntimeException => println("HMM or CRF evaluation failed " + e.toString)
  }
}

// --------------------------------  EOF -------------------------------
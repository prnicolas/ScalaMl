/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap5


import org.scalaml.app.ScalaMlApp


object Chap5 extends App with ScalaMlApp {
  private def runAll = {
	 BinomialBayesEval.run(Array[String]("0.5", "8"))
	 TextBayesEval.run
  }
	
  final val cmdDescriptor: String = {
	new StringBuilder("Command line: Chap 5 arg\n")
		  .append(" bayes: Evaluation Binomial Naive Bayes\n")
		  .append(" textBayes:  Evaluation Naive Bayes for text analysis\n")
		  .append(" all: All evaluation").toString
  }
  
  
  override protected def execute(args: Array[String]): String = {
     if( args == null || args.length == 0) "?" else args(0) match {
	  	 case "?" => cmdDescriptor
	  	 case "bayes" => {
	  		 BinomialBayesEval.run(Array[String]("0.5", "8"))
	  		 args(0)
	  	 }
	  	 case "textBayes" => TextBayesEval.run; args(0)
	  	 case "all" => runAll; args(0)
	  	 case _ => cmdDescriptor
	 }
  }

  process(args)
}

// -----------------------------  EOF ------------------------------------
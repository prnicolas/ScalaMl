/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap6

import org.scalaml.app.ScalaMlApp

object Chap6 extends App with ScalaMlApp {
	private def runAll = {
		SingleLinearRegressionEval.run()
		MultiLinearRegressionEval.run(Array[String]("trend"))
		MultiLinearRegressionEval.run(Array[String]("filter"))
		RidgeRegressionEval.run()
		LogisticRegressionEval.run()
	}
	
	final val cmdDescriptor: String = {
		new StringBuilder("Command line: Chap 6 arg\n")
		   .append(" singlelinear: Evaluation single linear regression\n")
		   .append(" multilinear:  Evaluation ordinary Least Squares regression\n")
		   .append(" ridge: Evaluation Ridge regression\n")
		   .append(" logistic: Evaluation Logistic regression\n")
		   .append(" all: All evaluation").toString
	}
	
    override protected def execute(args: Array[String]): String = {
       if( args == null || args.length == 0) "?" else args(0) match {
			case "?" => cmdDescriptor
			case "singlelinear" =>  SingleLinearRegressionEval.run(); args(0)
			case "multilinear" => {
				MultiLinearRegressionEval.run(Array[String]("trend"))
				MultiLinearRegressionEval.run(Array[String]("filter"))
				args(0)
			}
			case "ridge" => RidgeRegressionEval.run(); args(0)
			case "logistic" => LogisticRegressionEval.run(); args(0)
			case "all" => runAll; args(0)
			case _ => cmdDescriptor
       }
	}
    
    process(args)
}


// ----------------------------  EOF ----------------------------------
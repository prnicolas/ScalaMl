/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap6



object Chap6 extends App {
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
	
	val argument = if( args == null && args.length == 0) "?" else args(0)
	argument match {
		case "?" => println(cmdDescriptor)
		case "singlelinear" =>  SingleLinearRegressionEval.run()
		case "multilinear" => {
			MultiLinearRegressionEval.run(Array[String]("trend"))
			MultiLinearRegressionEval.run(Array[String]("filter"))
		}
		case "ridge" => RidgeRegressionEval.run()
		case "logistic" => LogisticRegressionEval.run()
		case "all" => runAll
		case _ =>  println(cmdDescriptor)
	}
}


// ----------------------------  EOF ----------------------------------
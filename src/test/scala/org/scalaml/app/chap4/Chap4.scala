/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap4


import org.scalaml.app.ScalaMlApp
import org.scalaml.trading.YahooFinancials
import org.scalaml.workflow.data.DataSource


trait UnsupervisedLearningEval {
   final val path = "resources/data/chap4/"
       
   def run(args: Array[String]): Unit
   protected val extractor = YahooFinancials.adjClose :: List[Array[String] =>Double]()
   protected def symbols = DataSource.listSymbols(path)
}


object Chap4 extends App with ScalaMlApp { 
	private def runAll = {
		KMeansEval.run(null)
		EMEval.run(Array[String]("2", "40"))
        EMEval.run(Array[String]("3", "25"))
        EMEval.run(Array[String]("4", "15"))
		PCAEval.run(null)
	}
		
    final val cmdDescriptor: String = {
		new StringBuilder("Command line: Chap 4 arg\n")
		   .append(" kmeans: Evaluation Kmeans clustering\n")
		   .append(" em:  Evaluation Expectation Maximization\n")
		   .append(" pca: Evaluation Principal Components Analysis\n")
		   .append(" all: All evaluation\n")
           .append(" ?: help)").toString
	}
	
    override protected def execute(args: Array[String]): String = {
	   if( args == null || args.length == 0) "?" else args(0) match {
			case "?" => cmdDescriptor
			case "kmeans" => {
				KMeansEval.run(Array[String]("2", "3", "4", "7", "9", "10", "13", "15"))
				args(0)
			}
			case "em" => {
			    EMEval.run(Array[String]("2", "40"))
	            EMEval.run(Array[String]("3", "25"))
	            EMEval.run(Array[String]("4", "15"))
	            args(0)
			}
			case "pca" =>  PCAEval.run(null); args(0)
			case "all" => runAll; args(0)
			case _ => cmdDescriptor
		}
	 }
    
     process(args)
}


// -----------------------------------  EOF ---------------------------------------------------
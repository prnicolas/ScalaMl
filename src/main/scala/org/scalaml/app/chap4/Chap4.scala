/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap4

import org.scalaml.core.{Types, XTSeries}
import org.scalaml.trading.PriceVolume
import org.scalaml.workflow.data.{DataSource, DataSink}
import PriceVolume._
import Types.ScalaMl._
import org.scalaml.unsupervised.pca.PCA


trait UnsupervisedLearningEval {
   final val path = "resources/data/chap4/"
       
   def run(args: Array[String]): Unit
   protected val extractor = PriceVolume.adjClose :: List[Array[String] =>Double]()
   protected def symbols = DataSource.listSymbols(path)
}


object Chap4 extends App { 
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
		   .append(" all: All evaluation").toString
	}
	
	val argument = if( args == null && args.length == 0) "?" else args(0)
	try {
		argument match {
			case "?" => println(cmdDescriptor)
			case "kmeans" => KMeansEval.run(Array[String]("2", "3", "4", "7", "9", "10", "13", "15"))
			case "em" => {
			    EMEval.run(Array[String]("2", "40"))
	            EMEval.run(Array[String]("3", "25"))
	            EMEval.run(Array[String]("4", "15"))
			}
			case "pca" =>  PCAEval.run(null)
			case "all" => runAll
			case _ =>  println(cmdDescriptor)
		}
	 }
	 catch {
  	  case e: RuntimeException =>  println("Runtime error " + e.toString); e.printStackTrace
    }
}


// -----------------------------------  EOF ---------------------------------------------------
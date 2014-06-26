package org.scalaml.app.chap5


import org.scalaml.supervised.bayes._
import org.scalaml.core.{Types, XTSeries}
import org.scalaml.workflow.data.{DataSource,TextSource}
import scala.collection.mutable.ArrayBuffer
import Types.ScalaMl._
import org.scalaml.filtering.SimpleMovingAverage
import SimpleMovingAverage._
import scala.collection.immutable.HashSet
import org.scalaml.supervised.bayes.MultinomialNaiveBayes


object Chap5 extends App {
  private def runAll = {
	 Dim2BayesEval.run(Array[String]("0.5", "8"))
	 TextBayesEval.run
  }
	
  final val cmdDescriptor: String = {
	new StringBuilder("Command line: Chap 5 arg\n")
		  .append(" bayes: Evaluation Multinomial Naive Bayes\n")
		  .append(" textBayes:  Evaluation Naive Bayes for text analysis\n")
		  .append(" all: All evaluation").toString
  }
  
  val argument = if( args == null && args.length == 0) "?" else args(0)
  try {
	  argument match {
	  	 case "?" => println(cmdDescriptor)
	  	 case "bayes" => Dim2BayesEval.run(Array[String]("0.5", "8"))
	  	 case "textBayes" => TextBayesEval.run
	  	 case "all" => runAll
	  	 case _ =>  println(cmdDescriptor)
	  }
  }
  catch {
  	 case e: RuntimeException =>  println("Runtime error " + e.toString); e.printStackTrace
  }
}

// -----------------------------  EOF ------------------------------------
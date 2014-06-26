/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap12


import org.scalaml.core.XTSeries
import scala.util.Random
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import com.typesafe.config.Config
import akka.actor.Props
import scala.concurrent.{Await, duration}
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.scalability.spark._
import org.apache.spark.SparkContext
import org.apache.spark.storage.StorageLevel
import org.scalaml.scalability.akka._
import org.scalaml.scalability.scala._



object Chap12 extends App {
    import java.util.concurrent.TimeoutException
	private def runAll = {
  	   ScalaParallelCollectionEval.run
       AkkaActorEval.run
	   AkkaFutureEval.run
	   SparkKMeansEval
   }
		
	final val cmdDescriptor: String = {
		new StringBuilder("Command line: Chap 2 arg\n")
		   .append(" scala: Evaluation of Scala parallel collections performance\n")
		   .append(" akka:  Evaluation of Akka framework\n")
		   .append(" spark:  Evaluation of Spark framework\n")
		   .append(" all: All evaluation").toString
	}
	
	val argument = if( args == null && args.length == 0) "?" else args(0)
	try {
		argument match {
			case "?" => println(cmdDescriptor)
			case "scala" =>  ScalaParallelCollectionEval.run
			case "akka" => {
			   AkkaActorEval.run
			   AkkaFutureEval.run
			}
			case "spark" => SparkKMeansEval
			case "all" => runAll
			case _ =>  println(cmdDescriptor)
		}	
	}
	catch {
		case e: TimeoutException  => println("Execution time out " + e.toString)
		case e: RuntimeException =>  println("Runtime error " + e.toString); e.printStackTrace
	}
}


// ---------------------------------  EOF -------------------------
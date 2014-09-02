/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.app.chap12


import org.scalaml.app.ScalaMlApp



object Chap12 extends App with ScalaMlApp {
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
	
	override protected def execute(args: Array[String]): String = {
		if( args == null || args.length == 0) "?" else args(0) match {
			case "?" => cmdDescriptor
			case "scala" =>  ScalaParallelCollectionEval.run; args(0)
			case "akka" => {
			   AkkaActorEval.run
			   AkkaFutureEval.run
			   args(0)
			}
			case "spark" => SparkKMeansEval; args(0)
			case "all" => runAll; args(0)
			case _ => cmdDescriptor
		}	
	}
	  
	process(args)
}


// ---------------------------------  EOF -------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap1

import scala.io.Source
import java.awt.Color
import org.scalaml.plots._
import org.scalaml.stats.Stats
import org.scalaml.trading.{Signal, PriceVolume}
import org.scalaml.core.Types
import Signal._
import Types.ScalaMl._
import org.scalaml.supervised.regression.logistic.LogBinRegression



		/**
		 * <p>Test driver for the techniques described in this chapter<br>
		 * <ul>
		 *   <li>JFreeChart plots</li>
		 *   <li>Logistic Binary classifier</li>
		 * </ul></p>
		 * @author Patrick Nicolas
		 * @data December 11, 2013
		 * @project Scala for Machine Learning.
		 */
object Chap1 extends App {
	private def runAll = {
  	   PlotterEval.run(args)
  	   LogBinRegressionEval.run(args)
    }
		
	final val cmdDescriptor: String = {
		new StringBuilder("Command line: Chap 1 args\n")
		   .append(" plotter: Evaluation of JFreeChart library\n")
		   .append(" test:  Evaluation of test case of logistic regression\n")
		   .append(" all: All evaluations").toString
	}
	
	val argument = if( args == null || args.length == 0) "?" else args(0)
	try {
		argument match {
			case "?" => println(cmdDescriptor)
			case "plotter" => PlotterEval.run(args)
			case "test" =>  LogBinRegressionEval.run(args)
			case "all" => runAll
			case _ =>  println(cmdDescriptor)
		}	
	}
	catch {
	   case e: RuntimeException =>  println("Runtime error " + e.toString); e.printStackTrace
	}
}

// --------------------  EOF --------------------------------------
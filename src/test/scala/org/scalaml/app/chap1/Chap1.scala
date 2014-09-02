/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.app.chap1

import scala.io.Source
import java.awt.Color
import org.scalaml.plots._
import org.scalaml.stats.Stats
import org.scalaml.trading.{Signal, YahooFinancials}
import org.scalaml.core.Types
import Signal._
import Types.ScalaMl._
import org.scalaml.supervised.regression.logistic.LogBinRegression
import scala.util.{Try, Success, Failure}
import org.scalaml.util.Display
import org.scalaml.app.ScalaMlApp





		/**
		 * <p>Test driver for the techniques described in this chapter<br>
		 * <ul>
		 *   <li>JFreeChart plots</li>
		 *   <li>Logistic Binary classifier</li>
		 * </ul></p>
		 * @author Patrick Nicolas
		 * @since December 11, 2013
		 * @note Scala for Machine Learning.
		 */
object Chap1 extends App with ScalaMlApp {
	
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

   override protected def execute(args: Array[String]): String = {
	  if( args == null || args.length == 0) "?" else args(0) match {
		  case "?" => cmdDescriptor
		  case "plotter" => PlotterEval.run(args); args(0)
		  case "test" =>  LogBinRegressionEval.run(args); args(0)
		  case "all" => runAll; args(0)
		  case _ => cmdDescriptor
	   }
   }
   
   process(args)
}

// --------------------  EOF --------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.app.chap2

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import org.scalaml.stats.{Stats, BiasVarianceEmulator}
import org.scalaml.workflow._
import scala.util.Random
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.plots.LightPlotTheme
import org.apache.log4j.Logger
import org.scalaml.app.ScalaMlApp




		/**
		 * <p>Test driver for the techniques described in this chapter<br>
		 * <ul>
		 *   <li>JDependency injection base workflow</li>
		 *   <li>Bias-Variance decomposition</li>
		 * </ul></p>
		 * 
		 * @author Patrick Nicolas
		 * @since February 1, 201t
		 * @note Scala for Machine Learning.
		 */
object Chap2 extends App with ScalaMlApp {

   private def runAll = {
  	 BiasVarianceEval.run
  	 WorkflowEval.run
   }
		
   final val cmdDescriptor: String = {
	  new StringBuilder("Command line: Chap 2 arg\n")
		   .append(" biasvariance: Evaluation of Bias-Variance decomposition\n")
		   .append(" workflow:  Evaluation of workflow\n")
		   .append(" all: All evaluation").toString
	}
	
  override protected def execute(args: Array[String]): String = {
	 if( args == null || args.length == 0) "?" else args(0) match {
		case "?" => cmdDescriptor
		case "biasvariance" => BiasVarianceEval.run; args(0)
		case "workflow" =>  WorkflowEval.run; args(0)
		case "all" => runAll; args(0)
		case _ => cmdDescriptor
	   }	
   }
  
   process(args)
}

     
// -------------------------------------  EOF -----------------------------------------
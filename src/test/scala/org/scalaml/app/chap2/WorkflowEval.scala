/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.app.chap2

import scala.util.Random
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import org.apache.log4j.Logger

import org.scalaml.stats.{Stats, BiasVarianceEmulator}
import org.scalaml.workflow._
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.core.Design.PipeOperator
import org.scalaml.plots.LightPlotTheme
import org.scalaml.util.DisplayUtils
import org.scalaml.app.Eval


	/**
	 * <p><b>Purpose</b>Singleton to evaluate the monadic data transformation using 
	 * dependency injection</p>
	 * 
	 * @author Patrick Nicolas
	 * @note Scala for Machine Learning Chapter 2
	 */
object TransformExample {
	private val logger = Logger.getLogger("TransformExample")
	
	val op1 = new PipeOperator[Int, Double] {
		override def |> : PartialFunction[Int, Double] = {
			case n: Int if(n > 0) => Math.sin(n.toDouble)
		}
	}
	
		/**
		 * <p>Execution of the scalatest for generic data transform. 
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run: Int = { 
		DisplayUtils.show(s"** TransformExample Example of Sinusoidal transformation", logger)
		val tform = new Transform[Int, Double](op1)
		DisplayUtils.show(s"TransformExample ${tform |> 6}", logger)
	}
}

		/**
		 * <p>Class to illustrate the dependency injection based framework 
		 * for building dynamic workflow. This first data transformation samples a function 
		 * f over the interval [0, 1].</p>
		 * @constructor Instantiate a Sampler data transformation. 
		 * @param Number of samples to be generated within the interval [0, 1]
		 * @throws IllegalArgumentException if samples is out of range
		 * @author Patrick Nicolas 
		 * @since January 22, 2014
		 */
final class Sampler(val samples: Int) extends PipeOperator[Double => Double, DblVector] {
	require(samples > 0 && samples < 1E+5, s"Sampler: the number of samples $samples is out of range")
	  	  
	override def |> : PartialFunction[(Double => Double), DblVector] = { 
		case f: (Double => Double) => Array.tabulate(samples)(n => f(n.toDouble/samples)) 
	}
}

		/**
		 * <p>Normalizer class to illustrate the dependency injection based framework 
		 * for building dynamic workflow. This data transformation consists of normalizing
		 * the sample data.
		 * @author Patrick Nicolas 
		 * @since January 22, 2014
		 */
final class Normalizer extends PipeOperator[DblVector, DblVector] {
	override def |> : PartialFunction[DblVector, DblVector] = { 
		case x: DblVector if( !x.isEmpty) => Stats[Double](x).normalize
	}
}

		/**
		 * <p>A reducer class to illustrate the dependency injection based framework 
		 * for building dynamic workflow. The simple purpose of the class is to extract 
		 * the index of the sample with the highest value (1.0)
		 * @author Patrick Nicolas 
		 * @since January 22, 2014
		 */
final class Reducer extends PipeOperator[DblVector, Int] { 
	override def |> : PartialFunction[DblVector, Int] = { 
		case x: DblVector if( !x.isEmpty) => Range(0, x.size).find(x(_) == 1.0).get
	}
}



	/**
	 * <p>Singleton to evaluate the Dependency injection based workflow class.</p>
	 * 
	 * @author Patrick Nicolas
	 * @since February 3, 2014
	 * @note Scala for Machine Learning
	 */
object WorkflowEval extends Eval {
	import scala.util.{Try, Success, Failure}
  		/**
		 * Name of the evaluation 
		 */
	val name: String = "WorkflowEval"
	
		/**
		 * <p>Execution of the scalatest for dependency injection. 
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	override def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Evaluation for 'log(1+x) + noise' transform", logger)
		
		val g = (x: Double) => Math.log(x + 1.0) + Random.nextDouble
		val workflow = new Workflow[Double => Double, DblVector, DblVector, Int] 
							with PreprocModule[Double => Double, DblVector] 
								with ProcModule[DblVector, DblVector] 
									with PostprocModule[DblVector, Int] {
			
			val preProc: PipeOperator[Double => Double, DblVector] = new Sampler(100)
			val proc: PipeOperator[DblVector, DblVector] = new Normalizer
			val postProc: PipeOperator[DblVector, Int] = new Reducer
		}
		Try (workflow |> g ) match {
			case Success(result) => DisplayUtils.show(s"$name with index $result", logger)
			case Failure(e) => failureHandler(e)
		}
		
	}
}

     
// -------------------------------------  EOF -----------------------------------------
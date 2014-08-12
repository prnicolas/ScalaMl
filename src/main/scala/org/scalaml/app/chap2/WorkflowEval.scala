/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap2

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import org.scalaml.stats.{Stats, BiasVarianceEmulator}
import org.scalaml.workflow._
import scala.util.Random
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.plots.LightPlotTheme




	/**
	 * <p>Singleton to test the Transform monadic representation of the Pipe Operator</p>
	 * 
	 * @author Patrick Nicolas
	 * @since February 4, 2014
	 */
object TransformExample {
	val op1 = new PipeOperator[Int, Double] {
		def |> (n: Int): Option[Double] = Some(Math.sin(n.toDouble))
	}
	
	def run: Unit = { 
		println("Evaluation of workflow framework")
		
		val tform = new Transform[Int, Double](op1)
		tform |> 6 match {
			case Some(res) => println(res)
			case None => {}
		}
		
		def g(f: Int =>Option[Double]): (Int=> Long) = { 
			(n: Int) => {
				f(n) match {
					case Some(x) => x.toLong
					case None => -1L
				}
			}
		}
	    val z : Long = tform.map(f => g(f)).apply(4)
	}
}

		/**
		 * <p>Demonstration class to illustrate the dependency injection based framework 
		 * for building dynamic workflow. This first data transformation samples a function 
		 * f over the interval [0, 1].</p>
		 * @param Number of samples to be generated within the interval [0, 1]
		 * @throws IllegalArgumentException if samples is out of range
		 * @author Patrick Nicolas 
		 * @since January 22, 2014
		 */
final class Sampler(val samples: Int) extends PipeOperator[Double => Double, DblVector] {
  require(samples > 0 && samples < 1E+5, "Number of samples " + samples + " is out of range")
	  	  
   override def |> (f: Double => Double): Option[DblVector] = { 
      require( f != null, "sampler operation undefined")
      
  	  Some(Array.tabulate(samples)(n => f(n.toDouble/samples)) )
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
	
   override def |> (data: DblVector): Option[DblVector] = { 
  	 require(data != null && data.size > 1, "Input to normalizer undefined")
  	 Some(Stats[Double](data).normalize)
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
	   
  override def |> (data: DblVector): Option[Int] = { 
  	 require(data != null && data.size > 1, "Input to normalizer undefined")
  	 Range(0, data.size) find( data(_) == 1.0)
  }
}



	/**
	 * <p>Singleton to evaluate the Dependency injection based workflow class.</p>
	 * 
	 * @author Patrick Nicolas
	 * @since February 3, 2014
	 * @note Scala for Machine Learning
	 */
object WorkflowEval {
	def run: Unit = {
		val workflow = new Workflow[Double => Double, DblVector, DblVector, Int] 
		                         with PreprocModule[Double => Double, DblVector] 
		                                  with ProcModule[DblVector, DblVector] 
		                                        with PostprocModule[DblVector, Int] {
			
			val preProc: PipeOperator[Double => Double, DblVector] = new Sampler(100)
			val proc: PipeOperator[DblVector, DblVector] = new Normalizer
			val postProc: PipeOperator[DblVector, Int] = new Reducer
		}
		
		workflow |> ((x: Double) => Math.log(x + 1.0) + Random.nextDouble) match {
			case Some(index) => println(index)
			case None => println("no found")
		}
	}
}

     
// -------------------------------------  EOF -----------------------------------------
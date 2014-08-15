/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.workflow.module

import scala.Array.fallbackCanBuildFrom
import org.scalaml.workflow.PipeOperator
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._



trait PreprocessingModule[T] {
  implicit val convert = (t: T) => Double
  val preprocessor: Preprocessing[T]
  
  abstract class Preprocessing[T] {
  	  def execute(xt: XTSeries[T]): Unit
  }
  
  abstract class MovingAverage[T] extends Preprocessing[T] with PipeOperator[XTSeries[T], XTSeries[Double]]  {
  	  override def execute(xt: XTSeries[T]): Unit = this |> xt match {
  	  	case Some(filteredData) => filteredData.foreach(println)
  	  	case None => println("Filtering algorithm failed")
  	  }
  }

  class SimpleMovingAverage[@specialized(Double) T <% Double](val period: Int)(implicit num: Numeric[T]) extends MovingAverage[T] {
	   override def |> (xt: XTSeries[T]): Option[XTSeries[Double]] = None
  }
  
  class DFTFir[T <% Double](val g: Double=>Double) extends Preprocessing[T] with PipeOperator[XTSeries[T], XTSeries[Double]]  {

	  override def |> (xt: XTSeries[T]) : Option[XTSeries[Double]] = None
	  override def execute(xt: XTSeries[T]): Unit = this |> xt match {
  	  	case Some(filteredData) => filteredData.foreach(println)
  	  	case None => println("Filtering algorithm failed")
  	  }
  }
  
}


// ----------------------------------  EOF -------------------------------------
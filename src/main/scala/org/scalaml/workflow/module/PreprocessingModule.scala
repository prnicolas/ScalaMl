/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.workflow.module

import scala.Array.fallbackCanBuildFrom
import org.scalaml.core.design.PipeOperator
import org.scalaml.core.XTSeries
import org.scalaml.core.types.ScalaMl._



trait PreprocessingModule[T] {
  type DblSeries = XTSeries[Double]
  implicit val convert = (t: T) => Double
  val preprocessor: Preprocessing[T]
  
  abstract class Preprocessing[T] {
  	  def execute(xt: XTSeries[T]): Unit
  }
  
  abstract class MovingAverage[T] extends Preprocessing[T] with PipeOperator[XTSeries[T], DblSeries]  {
  	  override def execute(xt: XTSeries[T]): Unit = this |> xt
  }

  class SimpleMovingAverage[T <% Double](period: Int)(implicit num: Numeric[T]) extends MovingAverage[T] {
	  override def |> : PartialFunction[XTSeries[T], DblSeries] = { case _ => null }
  }
  
  class DFTFir[T <% Double](g: Double=>Double) extends Preprocessing[T] with PipeOperator[XTSeries[T], DblSeries]  {
	  override def |> : PartialFunction[XTSeries[T], DblSeries] = { case _ => null }
	  override def execute(xt: XTSeries[T]): Unit = (this |> xt).foreach(println)
  }
  
}


// ----------------------------------  EOF -------------------------------------
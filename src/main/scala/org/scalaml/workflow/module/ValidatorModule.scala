/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.workflow.module

import org.scalaml.workflow.Transform
import org.scalaml.core.Monad
import org.scalaml.util.Counter


object Label extends Enumeration {
  type Label = Value
  val TP, TN, FP, FN = Value
 }

/*
trait ValidatorModule[T] {
  val validator: Validator[T]
}
* 
*/

/*
import Label._
abstract class Validator[T](implicit val ctx: T => Array[Label]) extends Transform[T, Array[Label]](ctx) { 
   def |> (data: T): Double
}
* 
*/
 
  
/*
final class F2Validator[T](implicit val _ctx: T => Array[Label]) extends Validator[T] {
  require( _ctx != null, "Cannot validate undefined data");
  	 
  private[this] val counters = new Counter[Label]

  lazy val accuracy = {
  	 val num = counters.getOrElse(TP, 0) + counters.getOrElse(TN, 0);
  	  num.toDouble/counters.foldLeft(0)( (s,kv)  => s + kv._2)
  }
  	 
  lazy val precision = counters.getOrElse(TP, 0).toDouble/(counters.getOrElse(TP, 0) + counters.getOrElse(FP, 0))
  	 
  lazy val recall = counters.getOrElse(TP, 0).toDouble/(counters.getOrElse(TP, 0) + counters.getOrElse(FN, 0))
  	 
  override def |>(data: T) : Double  = {
  	 _ctx(data).foldLeft(counters)((cnt, lbl) => cnt + lbl  )
  	  2.0*precision*recall/(precision + recall)
  }
}
* 
*/

/*
import Label._
object ValidationModuleApp extends App {
	val values = Array[Double](0.0, 1.0, 4.0, 5.0, 7.0, 1.2, 12.8, 11.9, 4.8)
	val labels = Array[Label](Label.TN,Label.TP,Label.FN, Label.FN, Label.FN, Label.TN,Label.TP,Label.FP,Label.TP)
	
	val validationModule = new ValidatorModule[Array[Double]] {
		val validator = new F2Validator[Array[Double]](values, labels)
	}
	
	
	println("Precision: " + validationModule.validator.precision)
	println("Recall: " + validationModule.validator.recall)
}
* 
*/


// -----------------------  EOF -----------------------------------
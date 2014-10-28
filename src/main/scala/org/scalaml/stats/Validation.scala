 /**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.94
 */
package org.scalaml.stats


import org.scalaml.util.Counter
import org.scalaml.core.XTSeries


object Label extends Enumeration {
  type Label = Value
   val TP, TN, FP, FN = Value
}

	/**
	 * <p>Generic trait for the validation method, f1 to compute the F1-measure
	 * and the precision, recall tuple.</p>
	 * @author Patrick Nicolas
	 * @since January 29, 2014
	 * @note Scala for Machine Learning
	 */
trait Validation {
		/**
		 * Compute the F1 measure as (precision + recall)/(2.precision.recall)
		 */
	def f1: Double
	    /**
	     * Compute the precision and recall tuple using the counters for 
	     * True positive, true negative, False positive and false negatives results
	     */
	def precisionRecall: (Double, Double)
}

import Label._

		/**
		 * <p>Immutable class that implements the Validation variables on a results
		 * of a test run. The counters for TP, TN, FP and FN are computed during instantiation
		 * to the class, Accuracy, precision and recall are computed at run-time (lazy values).</p>
		 * @param actualExpected array of pair (actual value, labeled/expected value)
		 * @param tpClass that defined the true positive results
		 * @throws IllegalArgumentException if actualExpected is undefined or has no elements
		 * @author Patrick Nicolas
		 * @since February 1, 2014
		 * @note Scala for Machine Learning
		 */
class ClassValidation(actualExpected: Array[(Int, Int)], tpClass: Int) extends Validation {
   require(actualExpected != null && actualExpected.size > 0, "Cannot validate undefined results")
   require(tpClass >= 0, "Cannot validate a model with negative index for the target class") 
   
   private[this] val counters = actualExpected.foldLeft(new Counter[Label])((cnt, oSeries) => cnt + classify(oSeries._1, oSeries._2))

   		/**
   		 * Accuracy of a classifier using TP and TN counters.
   		 */
   lazy val accuracy = {
  	  val num = counters(TP) + counters(TN)
  	  num.toDouble/counters.foldLeft(0)( (s,kv)  => s + kv._2)
  	}
  	 
  	lazy val precision = counters(TP).toDouble/(counters(TP) + counters(FP))
  	 
  	lazy val recall = counters(TP).toDouble/(counters(TP) + counters(FN))
  	 
  		/**
		 * Compute the F1 measure as (precision + recall)/(2.precision.recall)
		 */
 	override def f1: Double  = 2.0*precision*recall/(precision + recall)
  			    
 	    /**
	     * Compute the precision and recall tuple using the counters for 
	     * True positive, true negative, False positive and false negatives results
	     */
  	override def precisionRecall: (Double, Double) = (precision, recall)

  	private def classify(val1: Int, val2: Int): Label = {
  	  if(val1 == val2) { if (val1 == tpClass) TP else TN }
  	  else  { if (val1 == tpClass) FP else FN }
  	}
}


		/**
		 * Companion object to the Class validation class that implement the constructors apply
		 * @author Patrick Nicolas
		 * @since February 1, 2014
		 * @note Scala for Machine Learning
		 */
object ClassValidation {
  def apply(data: Array[(Int, Int)], tpClass: Int): ClassValidation = new ClassValidation(data, tpClass)
  def apply(data: XTSeries[(Int, Int)], tpClass: Int): ClassValidation = new ClassValidation(data.toArray, tpClass)
}


// --------------------  EOF --------------------------------------------------------
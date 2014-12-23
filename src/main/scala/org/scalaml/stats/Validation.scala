 /**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.stats

import org.scalaml.util.MapUtils.Counter
import org.scalaml.core.XTSeries

		/**
		 * <p>Enumeration for validation labels such as TP (true positive), TN (true negative)
		 * FP (false positive) and FN (false negative)
		 */
object Label extends Enumeration {
	type Label = Value
			val TP, TN, FP, FN = Value
}

		/**
		 * <p>Generic trait for the validation method, f1 to compute the F1-measure
		 * and the precision, recall tuple.</p>
		 * @author Patrick Nicolas
		 * @since January 29, 2014
		 * @note Scala for Machine Learning Chapter 2 Hello World! / Assessing a model / Validation
		 */
trait Validation {
		/**
		 * Compute the F1 measure as (precision + recall)/(2.precision.recall)
		 * @return F1 measure
		 */
	def f1: Double

		/**
		 * Compute the precision and recall tuple using the counters for 
		 * True positive, true negative, False positive and false negatives results
		 * @return The tuple (precision, recall)
		 */
	def precisionRecall: (Double, Double)
}

import Label._

		/**
		 * <p>Immutable class that implements the Validation variables on a results
		 * of a test run. The counters for TP, TN, FP and FN are computed during instantiation
		 * to the class, Accuracy, precision and recall are computed at run-time (lazy values).</p>
		 * @constructor Create a class validation instance that compute precision, recall and F1 measure 
		 * @throws IllegalArgumentException if actualExpected is undefined or has no elements or 
		 * tpClass is out of range
		 * @param actualExpected Array of pair (actual value, labeled/expected value)
		 * @param tpClass Identifier for the class that defined the true positive samples
	
		 * @author Patrick Nicolas
		 * @since February 1, 2014
		 * @note Scala for Machine Learning Chapter 2 Hello World! / Assessing a model / Validation
		 */
final protected class ClassValidation(
		actualExpected: Array[(Int, Int)], 
		tpClass: Int) extends Validation {
  
	require( !actualExpected.isEmpty, 
			"ClassValidation Cannot validate undefined results")
	require(tpClass >= 0, s"ClassValidation index for true positive class $tpClass is negative") 
   
	private[this] val counters = 
		actualExpected.foldLeft(new Counter[Label])((cnt, oSeries) => 
			cnt + classify(oSeries._1, oSeries._2))

		/**
		 * Accuracy of a classifier using TP and TN counters.
		 * @return Accuracy for the model
		 */
	lazy val accuracy: Double = {
		val num = counters(TP) + counters(TN)
		num.toDouble/counters.foldLeft(0)( (s,kv)  => s + kv._2)
	}

  	 
		/**
		 * Precision of a classifier using TP and FP counters.
		 * @return Precision for the model if either TP or FP counters is not null
		 * @throws IllegalStateException if TP + FP counters are null
		 */
	lazy val precision = {
		val denom = counters(TP) + counters(FP)
		assert(denom > 0, "ClassValidation.precision TP and FP counters are null")
		counters(TP).toDouble/denom
	}
	
  	 
		/**
		 * Recall of a classifier using TP and FN counters.
		 * @return Recall for the model
		 */
	lazy val recall = {
		val denom = counters(TP) + counters(FN)
		assert(denom > 0, "ClassValidation.recall TP and FP counters are null")
		counters(TP).toDouble/denom
	}
  	 
		/**
		 * Compute the F1 measure as (precision + recall)/(2.precision.recall)
		 */
 	override def f1: Double  = 2.0*precision*recall/(precision + recall)
  			    
 		/**
 		 * Compute the precision and recall tuple using the counters for 
 		 * True positive, true negative, False positive and false negatives results
 		 * @return F1 measure for this classifier
 		 */
 	override def precisionRecall: (Double, Double) = (precision, recall)


 	private def classify(val1: Int, val2: Int): Label = {
 		if(val1 == val2) 
 			if (val1 == tpClass) TP else TN 
 		else 
 			if (val1 == tpClass) FP else FN
 	}
}


		/**
		 * Companion object to the Class validation class that implement the constructors apply
		 * @author Patrick Nicolas
		 * @since February 1, 2014
		 * @note Scala for Machine Learning Chapter 2 Hello World! / Assessing a model / Validation
		 */
object ClassValidation {
		/**
		 * Default constructor for the ClassValidation
		 * @param actualExpected Array of pair (actual value, labeled/expected value)
		 * @param tpClass Identifier for the class that defined the true positive samples
		 */
 	def apply(actualExpected: Array[(Int, Int)], tpClass: Int): ClassValidation = 
 		new ClassValidation(actualExpected, tpClass)

 		/**
		 * Variant of the constructor for the ClassValidation that uses a time series of tuples
		 * expected, actual values as input
		 * @param actualExpected Time series pair (actual value, labeled/expected value)
		 * @param tpClass Identifier for the class that defined the true positive samples
		 */
 	def apply(actualExpected: XTSeries[(Int, Int)], tpClass: Int): ClassValidation = 
 		new ClassValidation(actualExpected.toArray, tpClass)
}


// --------------------  EOF --------------------------------------------------------
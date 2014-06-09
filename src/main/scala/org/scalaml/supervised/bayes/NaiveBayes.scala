/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.supervised.bayes

import org.scalaml.core.{XTSeries, Types}
import org.scalaml.stats.Validation
import scala.collection.mutable.ArraySeq
import scala.annotation.implicitNotFound
import org.scalaml.stats.Stats
import Types._
import org.scalaml.workflow.PipeOperator
import org.scalaml.supervised.Supervised
import NaiveBayesModel._


import Stats._
import org.scalaml.stats.ClassValidation



    
    /**
     * <p>Generic Multinomial Naive Bayes classification class. The class is used for both training
     * and run-time classification.
     */
class MultinomialNaiveBayes[T <% Double](val smoothing: Double, val lblSeries: XTSeries[(Array[T], Int)], val density: Density)
                              extends PipeOperator[XTSeries[Array[T]], Array[Int]]
                                 with Supervised[T] {
	import XTSeries._, Types.ScalaMl._
	val model = BinNaiveBayesModel[T](train(lblSeries.arr, 1), train(lblSeries.arr, 0), density)

		/**
		 * Run-time classifier
		 */
	override def |> (xt: XTSeries[Array[T]]): Option[Array[Int]] =  Some( xt.arr.map( model.classify( _)) )
	
	
	override def validate(xt: XTSeries[(Array[T], Int)], index: Int): Double = 
       ClassValidation( xt.map(x =>(model.classify(x._1), x._2)) , index).f1
	
    
    @implicitNotFound("Conversion from array of parameterize type to DblVector not defined for training NB")
	private def train(labels: Array[(Array[T], Int)], label: Int)(implicit f: Array[T] => DblVector): Prior[T] = {
	   val values = labels.filter( _._2 == label).map(x => f(x._1))
	   val dim = labels(0)._1.size
	   val vSeries = XTSeries[DblVector](values.toArray)

	   Prior(label, statistics(vSeries).map(stat => (stat.lidstoneMean(smoothing, dim), stat.stdDev) ), values.size.toDouble/labels.size) 
	}
		

	override def toString: String = model.toString
}


object MultinomialNaiveBayes {
	def apply[T <% Double](smoothing: Double, labelSeries: XTSeries[(Array[T], Int)], density: Density) = new MultinomialNaiveBayes[T](smoothing, labelSeries, density)
    def apply[T <% Double](labelSeries: XTSeries[(Array[T], Int)]) = new MultinomialNaiveBayes[T](1.0, labelSeries, gauss)
}


// ------------------------------  EOF --------------------------------------------
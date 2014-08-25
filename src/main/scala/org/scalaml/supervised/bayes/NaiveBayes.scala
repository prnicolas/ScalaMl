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
     * <p>Generic Multi-nomial Naive Bayes classification class. The class is used for both training
     * and run-time classification. The training of the model is executed during the instantiation
     * of the class to avoid having an uninitialized model.<br>
     * As a classifier, the method implement the generic data transformation PipeOperator and the
     * Supervised interface.</p>
     * @param smoothing Laplace or Lidstone smoothing factor
     * @param xt Input labeled time series used for training
     * @param density Density function used to compute the discriminant
     * @throws IllegalArgumentException if one of the class parameters is undefined
     * 
     * @author Patrick Nicolas
     * @since February 13, 2014
     * @note Scala for Machine learning
     */
final protected class NaiveBayes[T <% Double](val smoothing: Double, val xt: XTSeries[(Array[T], Int)], val density: Density)
                              extends PipeOperator[XTSeries[Array[T]], Array[Int]]
                                 with Supervised[T] {
	import XTSeries._, Types.ScalaMl._
	validate(smoothing, xt, density)

	private val model = BinNaiveBayesModel[T](train(1), train(0), density)

		/**
		 * <p>Run-time classification of a time series using the Naive Bayes model</p>
		 */
	override def |> (xt: XTSeries[Array[T]]): Option[Array[Int]] =  (xt != null && xt.size > 0) match {
		case true =>  Some(xt.toArray.map( model.classify( _)) )
		case false => None 
	}
	
	
	override def validate(xt: XTSeries[(Array[T], Int)], index: Int): Double = 
       ClassValidation( xt.map(x =>(model.classify(x._1), x._2)) , index).f1
	
       
    
	override def toString: String = model.toString   
       
    
    @implicitNotFound("Conversion from array of parameterize type to DblVector not defined for training NB")
	private def train(label: Int)(implicit f: Array[T] => DblVector): Likelihood[T] = {
	   val xi = xt.toArray
	   val values = xi.filter( _._2 == label).map(x => f(x._1))
	   val dim = xi(0)._1.size
	   val vSeries = XTSeries[DblVector](values.toArray)

	   Likelihood(label, 
	  		      statistics(vSeries).map(stat => (stat.lidstoneMean(smoothing, dim), stat.stdDev) ), 
	  		      values.size.toDouble/xi.size) 
	}	
	
    private def validate(smoothing: Double, xt: XTSeries[(Array[T], Int)], density: Density): Unit = {
		require(smoothing > 0.0 && smoothing <= 1.0, "Laplace or Lidstone smoothing factor " + smoothing + " is out of range")
		require(xt != null && xt.size > 0, "Time series input for training Naive Bayes is undefined")
		require(density != null, "Density function for Naive Bayes is undefined")
	}
}



		/**
		 * Singleton that define the constructors for the NaiveBayes classifier
		 */
object NaiveBayes {
	def apply[T <% Double](smoothing: Double, labelSeries: XTSeries[(Array[T], Int)], density: Density): NaiveBayes[T] 
	          = new NaiveBayes[T](smoothing, labelSeries, density)
    def apply[T <% Double](labelSeries: XTSeries[(Array[T], Int)]): NaiveBayes[T] 
	          = new NaiveBayes[T](1.0, labelSeries, gauss)
}


// ------------------------------  EOF --------------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95e
 */
package org.scalaml.supervised.bayes

import org.scalaml.core.XTSeries
import org.scalaml.core.types.ScalaMl._
import org.scalaml.stats.Validation
import scala.collection.mutable.ArraySeq
import scala.annotation.implicitNotFound
import org.scalaml.stats.Stats._
import org.scalaml.core.design.PipeOperator
import org.scalaml.supervised.Supervised
import org.scalaml.stats.ClassValidation
import org.apache.log4j.Logger
import scala.util.{Try, Success, Failure}
import org.scalaml.util.Display

import NaiveBayesModel._
import XTSeries._

    
    /**
     * <p>Generic Binomial Naive Bayes classification class. The class is used for both training
     * and run-time classification. The training of the model is executed during the instantiation
     * of the class to avoid having an uninitialized model. A conversion from a parameterized array, Array[T] 
     * to an array of double, DblVector, has to be implicitly defined for training the model.<br>
     * As a classifier, the method implement the generic data transformation PipeOperator and the
     * Supervised interface.</p>
     * @constructor Instantiate a parameterized NaiveBayes model [smoothing]: Laplace or Lidstone smoothing factor. [xt]: Input labeled time series used for training. [density]: Density function used to compute the discriminant
     * @param smoothing Laplace or Lidstone smoothing factor
     * @param xt Input labeled time series used for training
     * @param density Density function used to compute the discriminant
     * @throws IllegalArgumentException if one of the class parameters is undefined
     * 
     * @author Patrick Nicolas
     * @since February 13, 2014
     * @note Scala for Machine learning
     */
final class NaiveBayes[T <% Double](smoothing: Double, xt: XTSeries[(Array[T], Int)], density: Density)
                              extends PipeOperator[XTSeries[Array[T]], Array[Int]]
                                 with Supervised[T] {

	check(smoothing, xt, density)
	
	private val logger = Logger.getLogger("NaiveBayes")
	private[this] val model: Option[BinNaiveBayesModel[T]] = Try(BinNaiveBayesModel[T](train(1), train(0), density))
	                          match {
		                         case Success(nb) => Option(nb)
		                         case Failure(e) => Display.error("NaiveBayes.model", logger, e); None
	                          }
		/**
		 * <p>Run-time classification of a time series using the Naive Bayes model</p>
		 *  @throws MatchError if the input time series is undefined or have no elements or the model was not properly trained
   		  * @return PartialFunction of time series of elements of type T as input to the Naive Bayes and array of class indices as output
		 */
	override def |> : PartialFunction[XTSeries[Array[T]], Array[Int]] = {
	   case xt: XTSeries[Array[T]] if(xt != null && xt.size > 0 && model != None) => 
	      xt.toArray.map( model.get.classify( _))
	}
	
		/**
		 * <p>Compute the F1 statistics for the Naive Bayes.</p>
		 * @param xt Time series of features of type Array[T], and class indices as labels
		 * @param index of the class, the time series or observation should belong to
		 * @return F1 measure if the model has been properly trained (!= None), None otherwise
		 */
	override def validate(xt: XTSeries[(Array[T], Int)], index: Int): Option[Double] = 
	   if(model == None) None
	   else Some(ClassValidation(xt.map(x =>(model.get.classify(x._1), x._2)) , index).f1)
	
       
	override def toString: String = if(model != None) model.get.toString  else "No Naive Bayes model"
       
    	/**
    	 * Train the Naive Bayes model on one of the two classes (positive = 1) or negative (=0)
    	 */
    @implicitNotFound("NaiveBayes; Conversion from array[T] to DblVector is undefined")
	private def train(label: Int)(implicit f: Array[T] => DblVector): Likelihood[T] = {
	   val xi = xt.toArray
	   val values = xi.filter( _._2 == label).map(x => f(x._1))
	   val dim = xi(0)._1.size
	   val vSeries = XTSeries[DblVector](values.toArray)

	   Likelihood(label, 
	  		      statistics(vSeries).map(stat => (stat.lidstoneMean(smoothing, dim), stat.stdDev) ), 
	  		      values.size.toDouble/xi.size) 
	}	
	
    private def check(smoothing: Double, xt: XTSeries[(Array[T], Int)], density: Density): Unit = {
		require(smoothing > 0.0 && smoothing <= 1.0, s"NaiveBayes: Laplace or Lidstone smoothing factor $smoothing is out of range")
		require(xt != null && xt.size > 0, "NaiveBayes: Time series input for training Naive Bayes is undefined")
		require(density != null, "NaiveBayes: Density function for Naive Bayes is undefined")
	}
}



		/**
		 * Singleton that define the constructors for the NaiveBayes classifier
		 */
object NaiveBayes {	
	def apply[T <% Double](smoothing: Double, labelSeries: XTSeries[(Array[T], Int)], density: Density): NaiveBayes[T] 
	          = new NaiveBayes[T](smoothing, labelSeries, density)
    def apply[T](labelSeries: XTSeries[(Array[T], Int)])(implicit f: T => Double): NaiveBayes[T] 
	          = new NaiveBayes[T](1.0, labelSeries, gauss)
}


// ------------------------------  EOF --------------------------------------------
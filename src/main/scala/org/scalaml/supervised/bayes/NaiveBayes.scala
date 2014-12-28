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
package org.scalaml.supervised.bayes

// Scala classes
import scala.collection.mutable.ArraySeq
import scala.annotation.implicitNotFound
import scala.util.{Try, Success, Failure}

import org.apache.log4j.Logger

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.stats.{Validation, ClassValidation, Stats}
import org.scalaml.core.Design.PipeOperator
import org.scalaml.supervised.Supervised
import org.scalaml.util.DisplayUtils
import NaiveBayesModel._, XTSeries._, Stats._

    
		/**
		 * <p>Generic Binomial Naive Bayes classification class. The class is used for both training
		 * and run-time classification. The training of the model is executed during the instantiation
		 * of the class to avoid having an uninitialized model. A conversion from a parameterized 
		 * array, Array[T] to an array of double, DblVector, has to be implicitly defined for 
		 * training the model.<br>
		 * As a classifier, the method implement the generic data transformation PipeOperator and the
		 * Supervised interface.<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;
		 * ,Arial,Helvetica,sans-serif;">
		 *  Naive Bayes formula: p(C}x) = p(x|C).p(C)/p(x) => p(C|x) = p(x1|C).p(x2|C). .. p(xn|C).p(C)</span></pre></p> 
		 * @constructor Instantiate a parameterized NaiveBayes model 
		 * @param smoothing Laplace or Lidstone smoothing factor
		 * @param xt  Input labeled time series used for training
		 * @param density Density function used to compute the discriminant
		 * 
		 * @throws IllegalArgumentException if one of the class parameters is undefined
		 * @author Patrick Nicolas
		 * @since February 13, 2014
		 * @note Scala for Machine learning Chapter 5 Naive Bayes Models / Naive Bayes classifiers
		 */
final class NaiveBayes[T <% Double](
		smoothing: Double, 
		xt: XTSeries[(Array[T], Int)], 
		density: Density)	extends PipeOperator[XTSeries[Array[T]], Array[Int]] with Supervised[T] {

	import NaiveBayes._
	check(smoothing, xt)
	
	private val logger = Logger.getLogger("NaiveBayes")
	
		// The model is instantiated during training for both
		// classes if the training is successful. It is None otherwise
	private[this] val model: Option[BinNaiveBayesModel[T]] = 
			Try(BinNaiveBayesModel[T](train(1), train(0), density)) match {
		case Success(nb) => Some(nb)
		case Failure(e) => DisplayUtils.none("NaiveBayes.model", logger, e)
	}
		
		/**
		 * <p>Run-time classification of a time series using the Naive Bayes model. The method invoke
		 * the actual classification method in one of the NaiveBayes models.</p>
		 * @throws MatchError if the input time series is undefined or have no elements or the 
		 * model was not properly trained
		 * @return PartialFunction of time series of elements of type T as input to the Naive Bayes 
		 * and array of class indices as output
		 */
	override def |> : PartialFunction[XTSeries[Array[T]], Array[Int]] = {
		case xt: XTSeries[Array[T]] if( !xt.isEmpty && model != None) => 
			xt.toArray.map( model.get.classify( _))
	}
	
		/**
		 * <p>Compute the F1 statistics for the Naive Bayes.</p>
		 * @param xt Time series of features of type Array[T], and class indices as labels
		 * @param index of the class, the time series or observation should belong to
		 * @return F1 measure if the model has been properly trained (!= None), None otherwise
		 */
	override def validate(xt: XTSeries[(Array[T], Int)], index: Int): Option[Double] = model match {
		case Some(m) => Some(ClassValidation(xt.map(x =>(m.classify(x._1), x._2)) , index).f1)
		case None => DisplayUtils.none("NaiveBayes Model undefined", logger)
	}


		/**
		 * Textual representation of the Naive Bayes classifier with labels for features.
		 * It returns "No Naive Bayes model" if no model exists
		 * @return Stringized features with their label if model exists.
		 */
	def toString(labels: Array[String]): String = 
		model.map(m => if( labels.isEmpty ) m.toString else m.toString(labels))
				.getOrElse("No Naive Bayes model")

		/**
		 * Default textual representation of the Naive Bayes classifier with labels for features.
		 * It returns "No Naive Bayes model" if no model exists
		 * @return Stringized features with their label if model exists.
		 */
	override def toString: String = toString(Array.empty)
   
		/**
		 * Train the Naive Bayes model on one of the two classes (positive = 1) or negative (=0)
		 */
	@implicitNotFound("NaiveBayes; Conversion from array[T] to DblVector is undefined")
	private def train(label: Int)(implicit f: Array[T] => DblVector): Likelihood[T] = {
		val xi = xt.toArray 
				// Extract then filter each observation to be associated to a specific label.
				// The implicit conversion from Array of type T to Array of type Double is invoked
		val values = xi.filter( _._2 == label).map(x => f(x._1))
		assert( !values.isEmpty, "NaiveBayes.train Filtered value is undefined")
		
			// Gets the dimension of a feature
		val dim = xi(0)._1.size
		val vSeries = XTSeries[DblVector](values)
	
			// Create a likelihood instance for this class 'label'. The
			// tuple (mean, standard deviation) (2nd argument) is computed
			// by invoking XTSeries.statistics then the Lidstone mean adjustment.
			// The last argument, class likelihood p(C) is computed as the ratio of the
			// number of observations associated to this class/label over total number of observations.
		Likelihood(label, 
				statistics(vSeries).map(stat => (stat.lidstoneMean(smoothing, dim), stat.stdDev) ), 
				values.size.toDouble/xi.size) 
	}
}

		/**
		 * Singleton that define the constructors for the NaiveBayes classifier and
		 * validate its parameters
		 * @author Patrick Nicolas
		 * @since February 13, 2014
		 * @note Scala for Machine learning Chapter 5 Naive Bayes Model
		 */
object NaiveBayes {	
		/**
		 * Default constructor for the NaiveBayes class
		 * @param smoothing Laplace or Lidstone smoothing factor
		 * @param xt Input labeled time series used for training
		 * @param density Density function used to compute the discriminant
		 */
	def apply[T <% Double](
			smoothing: Double, 
			xt: XTSeries[(Array[T], Int)], 
			density: Density): NaiveBayes[T] = new NaiveBayes[T](smoothing, xt, density)
		
		/**
		 * Constructor for the NaiveBayes class with a Laplace smoothing function and
		 * a Gaussian density function.
		 * @param xt  Input labeled time series used for training
		 */
	def apply[T <% Double](xt: XTSeries[(Array[T], Int)]): NaiveBayes[T] = 
			new NaiveBayes[T](1.0, xt, gauss)
		
	/*
	def |>[T <% Double](): PartialFunction[XTSeries[Array[T]], Array[Int]] = {
		case xt: XTSeries[Array[T]] if(!xt.isEmpty && {
			model = 
		  model != None }) => 
			xt.toArray.map( model.get.classify( _))
	}
	* 
	*/
	def |>[T <% Double](model: Option[BinNaiveBayesModel[T]]): PartialFunction[XTSeries[Array[T]], Array[Int]] = {
		case xt: XTSeries[Array[T]] if( !xt.isEmpty && model != None) => 
			xt.toArray.map( model.get.classify( _))
	}
	
	
	
	private def check[T <% Double](smoothing: Double, xt: XTSeries[(Array[T], Int)]): Unit = {
		require(smoothing > 0.0 && smoothing <= 1.0, 
	  		s"NaiveBayes: Laplace or Lidstone smoothing factor $smoothing is out of range")
		require( !xt.isEmpty, 
				"NaiveBayes: Time series input for training Naive Bayes is undefined")
	}
}


// ------------------------------  EOF --------------------------------------------
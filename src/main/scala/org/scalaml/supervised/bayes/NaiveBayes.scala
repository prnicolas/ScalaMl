/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99.1
 */
package org.scalaml.supervised.bayes

// Scala classes
import scala.language.implicitConversions
import scala.util.Try

import org.apache.log4j.Logger

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.stats.{XTSeries, Stats}
import org.scalaml.validation.MultiFValidation
import org.scalaml.core.ITransform
import org.scalaml.supervised.Supervised
import org.scalaml.util.LoggingUtils._
import NaiveBayesModel._, XTSeries._, Stats._, NaiveBayes._

    
		/**
		 * Generic Binomial Naive Bayes classification class. The class is used for both training
		 * and run-time classification. The training of the model is executed during the instantiation
		 * of the class to avoid having an uninitialized model. A conversion from a parameterized 
		 * array, ''Array[T]'' to an array of double, ''DblArray'', has to be implicitly defined for 
		 * training the model.
		 * 
		 * The implemantation follows the standard design of supervised learning algorithm:
		 * - The classifier implements the '''ITransform''' implicit monadic data transformation
		 * - The constructor triggers the training of the classifier, making the model immutable
		 * - The classifier implements the '''Monitor''' interface to collect profile information for 
		 * debugging purpose
		 * 
		 * As a classifier, the method implement a data transformation '''ITransform''' with a model
		 * explicitly extracted from a training set 
		 * {{{
		 *   Naive Bayes formula: 
		 *   p(C}x) = p(x|C).p(C)/p(x) => p(C|x) = p(x1|C).p(x2|C). .. p(xn|C).p(C)
		 * }}}
		 * @tparam T type of features in each observation
		 * @constructor Instantiate a parameterized NaiveBayes model 
		 * @param smoothing Laplace or Lidstone smoothing factor
		 * @param xt  Input labeled time series used for training
		 * @param expected in the training of a model
		 * @param classes Number of classes used in the Naive Bayes
		 * 
		 * @throws IllegalArgumentException if one of the class parameters is undefined
		 * @author Patrick Nicolas
		 * @since 0.98 February 13, 2014
		 * @version 0.99.1
		 * @see Scala for Machine learning Chapter 5 "Naive Bayes Models" / Naive Bayes classifiers
		 * @see org.scalaml.core.ITransform
		 */
@throws(classOf[IllegalArgumentException])
private[scalaml] final class NaiveBayes[T <: AnyVal](
		smoothing: Double, 
		xt: XVSeries[T],
		expected: Vector[Int],
		logDensity: LogDensity,
		classes: Int)(implicit f: T => Double)
	extends ITransform[Array[T]](xt) with Supervised[T, Array[T]] with Monitor[T] {

  type V = Int
	check(smoothing, xt, expected, classes)
	
	protected val logger = Logger.getLogger("NaiveBayes")
	
		/**
		 * The model is instantiated during training for both
		 * classes if the training is successful. It is None otherwise
		 */
	val model: Option[NaiveBayesModel[T]] = Try {
		if( classes == 2) 
			BinNaiveBayesModel[T](train(1), train(0))
		else 
			MultiNaiveBayesModel[T](List.tabulate(classes)( train(_)))
	}
  ._toOption("NaiveBayes.train", logger)



		/**
		 * Run-time classification of a time series using the Naive Bayes model. The method invoke
		 * the actual classification method in one of the NaiveBayes models.
		 * @throws MatchError if the input time series is undefined or have no elements or the 
		 * model was not properly trained
		 * @return PartialFunction of time series of elements of type T as input to the Naive Bayes 
		 * and array of class indices as output
		 */
	override def |> : PartialFunction[Array[T], Try[V]] = {
		case x: Array[T] if x.length > 0 && model.isDefined =>
			Try( model.map(_.classify(x, logDensity)).get )
	}
	
		/**
		 * Compute the F1 statistics for the Naive Bayes.
		 * @param xt Time series of features of type Array[T], and class indices as labels
		 * @param expected expected value or label
		 * @return F1 measure if the model has been properly trained, Failure otherwise
		 */
	override def validate(xt: XVSeries[T], expected: Vector[V]): Try[Double] = Try {
		val predict = model.get.classify(_ : Array[T], logDensity)
		MultiFValidation[T](expected, xt, classes)(predict).score
	}
	  
		/**
		 * Compute the F1 statistics for the Naive Bayes.
		 * @param labeled Time series of features and expected values
		 * @return F1 measure if the model has been properly trained, Failure otherwise
		 */
  def validate(labeled: Vector[(Array[T], V)]): Try[Double] = Try {
	  val predict = model.get.classify(_: Array[T], logDensity)
	  MultiFValidation[T](labeled, classes)(predict).score
	}

		/**
		 * Textual representation of the Naive Bayes classifier with labels for features.
		 * It returns "No Naive Bayes model" if no model exists
		 * @return Stringized features with their label if model exists.
		 */
	def toString(labels: Array[String]): String = 
		if( model.isDefined) "No model"
		else 
			if(labels.length > 0) model.get.toString(labels) else model.get.toString
		

		/**
		 * Default textual representation of the Naive Bayes classifier with labels for features.
		 * It returns "No Naive Bayes model" if no model exists
		 * @return Stringized features with their label if model exists.
		 */
	override def toString: String = toString(Array.empty[String])
   
		/**
		 * Train the Naive Bayes model on one of the the classes
		 */
	@throws(classOf[IllegalStateException])
	private def train(index: Int): Likelihood[T] = {
		val xv: XVSeries[Double] = xt
		
				// Extract then filter each observation to be associated to a specific label.
				// The implicit conversion from Array of type T to Array of type Double is invoked
		val values = xv.zip(expected).filter( _._2 == index).map(_._1)
		if( values.isEmpty )
		  throw new IllegalStateException("NaiveBayes.train Filtered value is undefined")
	
			// Gets the dimension of a feature
		val dim = dimension(xv) 
		
			// Create a likelihood instance for this class 'label'. The
			// tuple (mean, standard deviation) (2nd argument) is computed
			// by invoking XTSeries.statistics then the Lidstone mean adjustment.
			// The last argument, class likelihood p(C) is computed as the ratio of the
			// number of observations associated to this class/label over total number of observations.
		Likelihood(index, 
			statistics(values).map(stat => (stat.lidstoneMean(smoothing, dim), stat.stdDev) ),  
			values.size.toDouble/xv.size) 
	}
}

		/**
		 * Singleton that define the constructors for the NaiveBayes classifier and
		 * validate its parameters
		 * @author Patrick Nicolas
		 * @since 0.98 February 13, 2014
		 * @version 0.98
		 * @see Scala for Machine learning Chapter 5 "Naive Bayes Model"
		 */
object NaiveBayes {	
  
		/**
		 * Implicit conversion from a NaiveBayes[T} to a Try[NaiveBayes[T}} type.
		 */
	implicit def naiveBayes2Try[T <: AnyVal](nb: NaiveBayes[T])(implicit f: T => Double): Try[NaiveBayes[T]] = Try(nb)
	

		/**
		 * Default constructor for the NaiveBayes class
		 * @tparam T type of features in the time series xt
		 * @param smoothing Laplace or Lidstone smoothing factor
		 * @param xt Input time series of observations used for training
		 * @param expected Input labeled time series used for training
		 * @param logDensity  logarithm formulation of the probability density function
		 * @param classes Number of classes used in the Naive Bayes model
		 */
	def apply[T <: AnyVal](
			smoothing: Double, 
			xt: XVSeries[T],
			expected: Vector[Int],
			logDensity: LogDensity,
			classes: Int)(implicit f: T => Double): NaiveBayes[T] = 
	   new NaiveBayes[T](smoothing, xt, expected, logDensity, classes)
	

		/**
		 * Constructor for the NaiveBayes class using single input data set {observations, expected}
		 * @tparam T type of features in the time series xt
		 * @param smoothing Laplace or Lidstone smoothing factor
		 * @param xty Input time series of pair (observations, expected outcome) used for training
		 * @param logDensity  logarithm formulation of the probability density function
		 * @param classes Number of classes used in the Naive Bayes model
		 */
	def apply[T <: AnyVal] (
			smoothing: Double, 
			xty: Vector[(Array[T], Int)],
			logDensity: LogDensity = logGauss,
			classes: Int = 2)(implicit f: T => Double): NaiveBayes[T] = {
	
		val xy: (Vector[Array[T]], Vector[Int]) = xty.unzip
		new NaiveBayes[T](smoothing, xy._1, xy._2, logDensity, classes)
	}

	  		
		/**
		 * Constructor for the Binomial NaiveBayes class using the Laplace smoothing factor, and the 
		 * gaussian distribution 
		 * @tparam T type of features in the time series xt
		 * @param xt Input time series of observations used for training
		 * @param expected Input labeled time series used for training
		 */
	def apply[T <: AnyVal](xt: XVSeries[T], expected: Vector[Int])(implicit f: T => Double): NaiveBayes[T] = 
		new NaiveBayes[T](1.0, xt, expected, logGauss, 2)

	
	
	private def check[T](	
			smoothing: Double, 
			xt: XVSeries[T], 
			expected: Vector[Int], 
			classes: Int): Unit = {
	  
		require(smoothing > 0.0 && smoothing <= 1.0, 
			s"NaiveBayes: Found smoothing $smoothing required 0 < smoothing <= 1")
		require( xt.nonEmpty,
			"NaiveBayes: Time series input for training Naive Bayes is undefined")
		require( expected.nonEmpty,
			"NaiveBayes: labeled values for training Naive Bayes is undefined")
		require( classes > 1, 
			s"NaiveBayes: Naive Bayes found $classes required classes > 1")
		require( xt.size == expected.size, 
			s"NaiveBayes: observations set ${xt.size} and labels set ${expected.size} should have same size")
	}
}


// ------------------------------  EOF --------------------------------------------
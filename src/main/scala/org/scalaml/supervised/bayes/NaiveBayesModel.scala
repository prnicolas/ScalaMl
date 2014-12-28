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

import org.scalaml.stats.Stats
import org.scalaml.core.Design.Model
import NaiveBayesModel._



		/**
		 * <b>Abstract class for all the Naive Bayes model. The purpose of the model is to 
		 * classify a new set of observations.</p>
		 * @constructor Create a generic Naive Bayes Model using a specific density.
		 * @param density Function used in computing the conditional probability <b>p(C|x)</b>
		 * @author Patrick Nicolas
		 * @since March 8, 2014
		 * @note Scala for Machine Learning Chapter 5 Naive Bayes Models
		 */
abstract class NaiveBayesModel[T <% Double](val density: Density) extends Model {

		/**
		 * <p>Classify a new observation (or vector) using the Multi-nomial Naive Bayes model.</p>
		 * @param x new observation
		 * @return the class ID the new observations has been classified.
		 * @throws IllegalArgumentException if any of the observation is undefined.
		 */
	def classify(x: Array[T]): Int
	def toString(labels: Array[String]): String 
}
		/**
		 * <p>Companion object to the abstract NaiveBayesModel class. This singleton 
		 * is used to define the signature of the Density function.
		 */
object NaiveBayesModel {
		/**
		 * <p>Signature of the Density function used in Naive Bayes Model.
		 */
	type Density= (Double*) => Double
}


		/**
		 * <p>Implements the binomial (or 2-class) Naive Bayes model with a likelihood for positive and 
		 * negative outcome and for a specific density function.</p>
		 * @constructor Instantiation of a Binomial Naive Bayes model. 
		 * @see org.scalaml.supervised.bayes.NaiveBayesModel
		 * @param positives  Priors for the class of positive outcomes.
		 * @param negatives  Priors for the class of negatives outcomes.
		 * @param density Probability density function used in computing the conditional 
		 * probability <b>p(C|x)</b>
		 * @author Patrick Nicolas
		 * @since February 11, 2014
		 * @note Scala for Machine Learning Chapter 5 Naive Bayes Models / Naive Bayes Classifiers
		 */
protected class BinNaiveBayesModel[T <% Double](
		positives: Likelihood[T], 
		negatives: Likelihood[T], 
		density: Density) extends NaiveBayesModel[T](density) {

		/**
		 * <p>Classify a new observation (features vector) using the Binomial Naive Bayes model.</p>
		 * @param x new observation
		 * @return 1 if the observation belongs to the positive class, 0 otherwise
		 * @throws IllegalArgumentException if any of the observation is undefined.
		 */
	override def classify(x: Array[T]): Int = {
		require( !x.isEmpty, 
				"BinNaiveBayesModel.classify Undefined observations")
		
		// Simply select one of the two classes with the highest log posterior probability
		if (positives.score(x, density) > negatives.score(x, density)) 1 else 0
	}

			
			
	
	override def toString(labels: Array[String]): String = {
		require( !labels.isEmpty, "BinNaiveBayesModel.toString Undefined labels")
		s"\nPositive class\n${positives.toString(labels)}\nNegative class\n${negatives.toString(labels)}"
	}
	
	override def toString: String = 
			s"\nPositive\n${positives.toString}\nNegative\n${negatives.toString}"
}


		/**
		 * <p>Companion object for the Binomial Naive Bayes Model. This singleton is used
		 * to define the constructor of the BinNaiveBayesModel class.</p>
		 * 		 
		 * @author Patrick Nicolas
		 * @since February 11, 2014
		 * @note Scala for Machine Learning  Chapter 5 Naive Bayes Models / Naive Bayes Classifiers
		 */
object BinNaiveBayesModel {
		/**
		 * Default constructor for the binary Naive Bayes model as instance of BinNaiveBayesModel
		 * @param positives  Priors for the class of positive outcomes.
		 * @param negatives  Priors for the class of negatives outcomes.
		 * @param density Probability density function used in computing the conditional probability <b>p(C|x)</b>
		 */
	def apply[T <% Double](
			positives: Likelihood[T], 
			negatives: Likelihood[T], 
			density: Density): BinNaiveBayesModel[T] = 
					new BinNaiveBayesModel(positives, negatives, density)
}

		/**
		 * <p>Defines a Multi-class (or multi-nomial) Naive Bayes model for n classes.
		 * The number of classes is defined as likelihoodSet.size. The binomial Naive Bayes model, 
		 * BinNaiveBayesModel, should be used for the two class problem.</p>
		 * @constructor Instantiates a multi-nomial Naive Bayes model (number classes > 2)
		 * @throws IllegalArgumentException if any of the class parameters is undefined
		 * @param likelihoodSet  List of likelihood or priors for every classes in the model.
		 * @param density Probability density function used in computing the conditional probability p(C|x)
		 * @author Patrick Nicolas
		 * @since February 11, 2014
		 * @note Scala for Machine Learning  Chapter 5 Naive Bayes Models / Naive Bayes Classifiers
		 */
protected class MultiNaiveBayesModel[T <% Double](
		likelihoodSet: List[Likelihood[T]], 
		density: Density) extends NaiveBayesModel[T](density) {
  
	require(!likelihoodSet.isEmpty, 
			"MultiNaiveBayesModel Cannot classify using Multi-NB with undefined classes")
  
		/**
		 * <p>Classify a new observation (or vector) using the Mult-inomial Naive Bayes model.</p>
		 * @param x new observation
		 * @return the class ID the new observations has been classified.
		 * @throws IllegalArgumentException if any of the observation is undefined.
		 */
	override def classify(x: Array[T]): Int = {
		require( !x.isEmpty, "MultiNaiveBayesModel.classify Vector input is undefined")
		
			// The classification is performed by ordering the class according to the
			// log of their posterior probability and selecting the top one (highest 
			// posterior probability)
		likelihoodSet.sortWith((p1, p2) => p1.score(x, density) > p2.score(x, density)).head.label
	}

	
	
	override def toString(labels: Array[String]): String = {
		require( !labels.isEmpty, "MultiNaiveBayesModel.toString Vector input is undefined")
	  		
		val buf = new StringBuilder
		likelihoodSet.zipWithIndex.foreach(l => {
			buf.append(s"\nclass ${l._2}: ${l._1.toString(labels)}")
		})
		buf.toString
	}
}

		/**
		 * Companion object for the multi-nomial Naive Bayes Model. The singleton
		 * is used to define the constructor of MultiNaiveBayesModel
		 * 
		 * @author Patrick Nicolas
		 * @since February 10, 2014
		 * @note Scala for Machine Learning
		 */
object MultiNaiveBayesModel {
		/**
		 * Default constructor for the multi-nomial Naive Bayes model as instance of MultiNaiveBayesModel
		 * @param likelihoodSet  List of likelihood or priors for every classes in the model.
		 * @param density Probability density function used in computing the conditional probability p(C|x)
		 */
	def apply[T <% Double](
			likelihoodSet: List[Likelihood[T]], 
			density: Density): MultiNaiveBayesModel[T] = 
					new MultiNaiveBayesModel[T](likelihoodSet, density)
}


// --------------------------------  EOF --------------------------------------------------------------
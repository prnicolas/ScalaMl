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
 * Version 0.99
 */
package org.scalaml.supervised.bayes

import org.scalaml.stats.Stats
import org.scalaml.core.Design.Model
import NaiveBayesModel._



		/**
		 * Implements the binomial (or 2-class) Naive Bayes model with a likelihood for positive and 
		 * negative outcome and for a specific density function.
		 * @constructor Instantiation of a Binomial Naive Bayes model. 
		 * @tparam T type of features in each observation
		 * @see org.scalaml.supervised.bayes.NaiveBayesModel
		 * @param pos  Priors for the class of positive outcomes.
		 * @param neg  Priors for the class of negatives outcomes.
		 * @param density Probability density function used in computing the conditional 
		 * probability '''p(C|x)'''
		 * @author Patrick Nicolas
		 * @since 0.98 February 11, 2014
		 * @version 0.98.1
		 * @see Scala for Machine Learning Chapter 5 "Naive Bayes Models" / Naive Bayes Classifiers
		 */
protected class BinNaiveBayesModel[T <: AnyVal](
		pos: Likelihood[T], 
		neg: Likelihood[T])(implicit f: T => Double) extends NaiveBayesModel[T] {

		/**
		 * Classify a new observation (features vector) using the Binomial Naive Bayes model.
		 * @param x new observation
		 * @return 1 if the observation belongs to the positive class, 0 otherwise
		 * @throws IllegalArgumentException if any of the observation is undefined.
		 */
	override def classify(x: Array[T], logDensity: LogDensity): Int = {
		require( x.length > 0, 
				"BinNaiveBayesModel.classify Undefined observations")
		
		   // Simply select one of the two classes with the 
			 // highest log posterior probability
		if (pos.score(x, logDensity) > neg.score(x, logDensity)) 1 else 0
	}

	
	override def likelihoods: Seq[Likelihood[T]] = List[Likelihood[T]](pos, neg)
	
			
	override def toString(labels: Array[String]): String = {
		require( labels.length > 0, "BinNaiveBayesModel.toString Undefined labels")
		s"\nPositive class\n${pos.toString(labels)}\nNegative class\n${neg.toString(labels)}"
	}
	
	override def toString: String = 
			s"\nPositive\n${pos.toString}\nNegative\n${neg.toString}"
}


		/**
		 * Companion object for the Binomial Naive Bayes Model. This singleton is used
		 * to define the constructor of the BinNaiveBayesModel class.
		 * 		 
		 * @author Patrick Nicolas
		 * @since 0.98 February 11, 2014
		 * @version 0.98
		 * @see Scala for Machine Learning  Chapter 5 "Naive Bayes Models" / Naive Bayes Classifiers
		 */
object BinNaiveBayesModel {
		/**
		 * Default constructor for the binary Naive Bayes model as instance of BinNaiveBayesModel
		 * @param pos  Priors for the class of positive outcomes.
		 * @param neg  Priors for the class of negatives outcomes.
		 * @param density Probability density function used in computing the conditional probability 
		 * '''p(C|x)'''
		 */
	def apply[T <: AnyVal](pos: Likelihood[T], neg: Likelihood[T])
			(implicit f: T => Double): BinNaiveBayesModel[T] = 
		new BinNaiveBayesModel(pos, neg)
}

	

// --------------------------------  EOF --------------------------------------------------------------
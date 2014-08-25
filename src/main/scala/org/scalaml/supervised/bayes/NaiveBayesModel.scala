/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.supervised.bayes

import org.scalaml.stats.Stats


import NaiveBayesModel._



	/**
	 * Abstract class for all the Naive Bayes model. The purpose of the model is to 
	 * classify a new set of observations.
	 * @param density Probabiliy density function used in computing the conditional probability p(C|x)
	 * 
	 * @author Patrick Nicolas
	 * @since March 8, 2014
	 * @note Scala for Machine Learning
	 */
abstract class NaiveBayesModel[T <% Double](val density: Density) {
	require(density != null, "Cannot compute conditional prob with NB for undefined prob density")
	def classify(values: Array[T]): Int
}


	/**
	 * <p>Companion object to the abstract NaiveBayesModel class. This singletong 
	 * is used to define the signature of the Density function.
	 */
object NaiveBayesModel {
	
	/**
	 * <p>Signature of the Density function used in Naive Bayes Model.
	 */
   type Density= (Double*) => Double
}


	/**
	 * Defines a 2-class Naive Bayes model
	 * @param positives Prior for the class of positive outcomes
	 * @param negatives Prior for the class of negatives outcomes
	 * @param density Probability density function used in computing the conditional probability p(C|x)
	 * @throws IllegalArgumentException if any of the class parameters is undefined
	 * @see org.scalaml.supervised.bayes.NaiveBayesModel
	 * @author Patrick Nicolas
	 * @since February 11, 2014
	 * @note Scala for Machine Learning
	 */
protected class BinNaiveBayesModel[T <% Double](val positives: Likelihood[T], 
		                                        val negatives: Likelihood[T], 
		                                        val _density: Density) extends NaiveBayesModel[T](_density) {
	require(positives !=null, "Cannot classify an observation with undefine positive labels")
    require(negatives !=null, "Cannot classify an observation with undefine negative labels")
    
    	/**
    	 * Classify a new observation (or vector)
    	 * @param x new observation
    	 * @return 1 if the observation belongs to the positive class, 0 otherwise
    	 * @throws IllegalArgumentException if any of the observation is undefined.
    	 */
	override def classify(x: Array[T]): Int = {
		require(x != null && x.size > 0, "Cannot NB classify an undefined observation")
		
		if (positives.score(x, density) > negatives.score(x, density)) 1 else 0
	}
	
	override def toString: String = new StringBuilder(positives.toString).append(negatives.toString).toString
}




	/**
	 * <p>Companion object for the Binomial Naive Bayes Model. This singleton is used
	 * to define the constructor of the BinNaiveBayesModel class.</p>
	 */
object BinNaiveBayesModel {
	def apply[T <% Double](positives: Likelihood[T], negatives: Likelihood[T], density: Density): BinNaiveBayesModel[T] = 
		new BinNaiveBayesModel(positives, negatives, density)
}



	/**
	 * <p>Defines a Multiclass (or multinomial) Naive Bayes model for n classes.The number of classes is defined
	 * as likelihoodSet.size. Use the binomial Naive Bayes model BinNaiveBayesModel for a two class problem.</p>
	 *
	 * @param likelihoodSet list of likelihood for all the classes
	 * @param _density Probability density function used in computing the conditional probability p(C|x)
	 * @throws IllegalArgumentException if any of the class parameters is undefined.
	 * @author Patrick Nicolas
	 * @since February 11, 2014
	 * @note Scala for Machine Learning
	 */
protected class MultiNaiveBayesModel[T <% Double](val likelihoodSet: List[Likelihood[T]], 
		                                          val _density: Density) extends NaiveBayesModel[T](_density) {
  require(likelihoodSet != null && likelihoodSet.size > 0, "Cannot classify using Multi-NB with undefined classes")
  
  		 /**
    	 * Classify a new observation (or vector) using the Multinomial Naive Bayes model
    	 * @param x new observation
    	 * @return the class ID the new observations has been classified.
    	 * @throws IllegalArgumentException if any of the observation is undefined.
    	 */
  override def classify(x: Array[T]): Int = 
    likelihoodSet.sortWith( (p1,p2) => p1.score(x, density) > p2.score(x, density)).head.label
}



		/**
		 * Companion object for the Multinomial Naive Bayes Model. The singleton
		 * is used to define the constructor of MultiNaiveBayesModel
		 * 
		 * @author Patrick Nicolas
		 * @since February 10, 2014
		 * @note Scala for Machine Learning
		 */
object MultiNaiveBayesModel {
   def apply[T <% Double](likelihoodSet: List[Likelihood[T]], density: Density): MultiNaiveBayesModel[T] 
     = new MultiNaiveBayesModel[T](likelihoodSet, density)
}




// --------------------------------  EOF --------------------------------------------------------------
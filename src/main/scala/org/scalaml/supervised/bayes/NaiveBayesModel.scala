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
		 * <p>Class that represents a prior statistics for Naive Bayes classifier.<br>
		 * The prior consists of a label (index), the mean of the prior of each dimension of the model,
		 * the standard deviation of the prior of each dimension of the model and the class likelyHood.
		 * The Naive Bayes assume that the dimension of the model are independent, making the log of 
		 * the prior additive.</p>
		 * @param label for a specific class or prior
		 * @param muSigma  array of (mean, standard deviation) of the prior observations for the model
		 * @param classLikelihood probability of occurrence for the class specified by the label.
		 * @exception IllegalArgumentException if the array of mean and standard deviation of the prior is undefined 
		 * of if the class likelihood is out of range ]0,1]
		 * 
		 * @author Patrick Nicolas
		 * @date March 11, 2014
		 * @project Scala for Machine Learning
		 */
case class Prior[T <% Double](val label: Int, val muSigma: Array[(Double, Double)], val classLikelihood: Double) {
  import Stats._
  
  require(muSigma != null && muSigma.size > 0, "Cannot create a prior for undefined historical mean and standard deviation")
  require( classLikelihood > 0.0  && classLikelihood <= 1.0, "Class likelihood for the NB prior " + classLikelihood + " is out of range")
  
  		/**
  		 * <p>Compute the log p(C|x of log of the conditional probability of the class given an observation obs and
  		 * a probability density distribution.</p>
  		 * @param obs parameterized observation 
  		 * @param density probability density function (default Gauss)
  		 * @exception IllegalArgumentException if the density is undefined or the observations are undefined
  		 * @return log of the conditional probability p(C|x)
  		 */
  def score(obs: Array[T], density: Density): Double = {
  	 require(obs != null && obs.size > 0, "Cannot compute conditional prob with NB for undefined observations")
  	 require(density != null, "Cannot compute conditional prob with NB for undefined prob density")
  	 
	 (obs, muSigma).zipped
	       .foldLeft(0.0)((post, xms) => post + Math.log(density(xms._2._1, xms._2._2, xms._1))) + Math.log(classLikelihood)
  }
  
  override def toString: String = 
	new StringBuilder("\n")
	      .append(label)
	          .append(": ")
	             .append(muSigma.foldLeft(new StringBuilder)((b, m) => b.append(" (").append(m._1).append(",").append(m._2).append(") ")).toString)
	                .append(" <")
	                    .append(classLikelihood) 
	                       .append(">").toString
}


	/**
	 * Abstract class for all the Naive Bayes model. The purpose of the model is to 
	 * classify a new set of observations.
	 * @param density Probabiliy density function used in computing the conditional probability p(C|x)
	 */
sealed abstract class NaiveBayesModel[T <% Double](val density: Density) {
	require(density != null, "Cannot compute conditional prob with NB for undefined prob density")
	def classify(values: Array[T]): Int
}

	/**
	 * Defines a 2-class Naive Bayes model
	 * @param positives Prior for the class of positive outcomes
	 * @param negatives Prior for the class of negatives outcomes
	 * @param density Probability density function used in computing the conditional probability p(C|x)
	 * @exception IllegalArgumentException if any of the class parameters is undefined.
	 * @author Patrick Nicolas
	 */
case class BinNaiveBayesModel[T <% Double](val positives: Prior[T], val negatives: Prior[T], val _density: Density) extends NaiveBayesModel[T](_density) {
	require(positives !=null, "Cannot classify an observation with undefine positive labels")
    require(negatives !=null, "Cannot classify an observation with undefine negative labels")
    
    	/**
    	 * Classify a new observation
    	 * @param values new observation
    	 * @return 1 if the observation belongs to the positive class, 0 otherwise
    	 * @exception IllegalArgumentException if any of the observation is undefined.
    	 */
	override def classify(values: Array[T]): Int = {
		require(values != null && values.size > 0, "Cannot NB classify an undefined observation")
		
		if (positives.score(values, density) > negatives.score(values, density)) 1 else 0
	}
	
	override def toString: String = new StringBuilder(positives.toString).append(negatives.toString).toString
}



	/**
	 * Defines a Multiclass Naive Bayes model
	 * @param classes list of prior for all the classes
	 * @param _density Probability density function used in computing the conditional probability p(C|x)
	 * @exception IllegalArgumentException if any of the class parameters is undefined.
	 * @author Patrick Nicolas
	 */
case class MultiNaiveBayesModel[T <% Double](val classes: List[Prior[T]], val _density: Density) extends NaiveBayesModel[T](_density) {
  require(classes != null && classes.size > 0, "Cannot classify using Multi-NB with undefined classes")
  
  override def classify(values: Array[T]): Int = 
    classes.sortWith( (p1,p2) => p1.score(values, density) > p2.score(values, density)).head.label
}


object NaiveBayesModel {
   type Density= (Double, Double, Double) => Double
}


// --------------------------------  EOF --------------------------------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.94
 */
package org.scalaml.supervised.bayes

import org.scalaml.stats.Stats


import NaiveBayesModel._

import Likelihood._

		/**
		 * <p>Class that represents a likelihood for each feature for Naive Bayes classifier.<br>
		 * The prior consists of a label (index), the mean of the prior of each dimension of the model,
		 * the standard deviation of the prior of each dimension of the model and the class likeliHood.<br>
		 * The Naive Bayes assume that the dimension of the model are independent, making the log of 
		 * the prior additive.</p>
		 * @constructor Create a likelihood for a specific class. [label] Name or label of the class or prior for which the likelihood is computed. [muSigma]  Array of tuples (mean, standard deviation) of the prior observations for the model. [prior] Probability of occurrence for the class specified by the label. 
		 * @param label for a specific class or prior
		 * @param muSigma  array of (mean, standard deviation) of the prior observations for the model
		 * @param prior probability of occurrence for the class specified by the label.
		 * @throws IllegalArgumentException if the array of mean and standard deviation of the likelihood is undefined 
		 * of if the class likelihood is out of range ]0,1]
		 * 
		 * @author Patrick Nicolas
		 * @since March 11, 2014
		 * @note Scala for Machine Learning
		 */
protected class Likelihood[T <% Double](val label: Int, val muSigma: Array[(Double, Double)], prior: Double) {
  import Stats._
  
  require(muSigma != null && muSigma.size > 0, "Likelihood Cannot create a likelihood for undefined historical mean and standard deviation")
  require(prior > 0.0  && prior <= 1.0, s"Likelihood Prior for the NB prior $prior is out of range")
  
  		/**
  		 * <p>Compute the log p(C|x of log of the conditional probability of the class given an observation obs and
  		 * a probability density distribution.</p>
  		 * @param obs parameterized observation 
  		 * @param density probability density function (default Gauss)
  		 * @throws IllegalArgumentException if the density is undefined or the observations are undefined
  		 * @return log of the conditional probability p(C|x)
  		 */
  final def score(obs: Array[T], density: Density): Double = {
  	 require(obs != null && obs.size > 0, "Likelihood.score Cannot compute conditional prob with NB for undefined observations")
  	 require(density != null, "Likelihood.score Cannot compute conditional prob with NB for undefined prob density")

	 (obs, muSigma).zipped
	               .foldLeft(0.0)((post, xms) => {
	    val probability = density(xms._2._1, xms._2._2, xms._1)
	    post + Math.log(if(probability< MIN_LOG_ARG) MIN_LOG_VALUE else probability)
	 }) + Math.log(prior)
  }

  override def toString: String = {
    val muSigmaStr = muSigma.foldLeft(new StringBuilder)((b, m) => b.append(s" (${m._1},${m._2}) ")).toString
     s"\nlabel: $label: ${muSigmaStr} [$prior]"
  }
}


	/**
	 * <p>Companion object for the Naive Bayes Likelihood class. The singleton
	 * is used to define the constructor apply for the class.</p>
	 * @author Patrick Nicolas
	 * @since March 11, 2014
	   @note Scala for Machine Learning
	 */

object Likelihood {
   val MIN_LOG_ARG = 1e-32
   val MIN_LOG_VALUE = -1e+32
   def apply[T <% Double](label: Int, muSigma: Array[(Double, Double)], prior: Double): Likelihood[T] 
    = new Likelihood[T](label, muSigma, prior)
}


// --------------------------------  EOF --------------------------------------------------------------
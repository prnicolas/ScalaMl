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
import org.scalaml.util.{FormatUtils, DisplayUtils}
import org.scalaml.core.Types.ScalaMl.XYTSeries
import NaiveBayesModel._

		/**
		 * <p>Class that represents a likelihood for each feature for Naive Bayes classifier.<br>
		 * The prior consists of a label (index), the mean of the prior of each dimension of the model,
		 * the standard deviation of the prior of each dimension of the model and the class likeliHood.<br>
		 * The Naive Bayes assume that the dimension of the model are independent, making the log of 
		 * the prior additive.</p> 
		 * @constructor Create a likelihood for a specific class. 
		 * @throws IllegalArgumentException if the array of mean and standard deviation of the 
		 * likelihood is undefined or if the class likelihood (prior) is out of range ]0,1]
		 * @param label  Name or label of the class or prior for which the likelihood is computed.
		 * @param muSigma Array of tuples (mean, standard deviation) of the prior observations for the model
		 * @param prior  Probability of occurrence for the class specified by the label.
		 * 
		 * @author Patrick Nicolas
		 * @since March 11, 2014
		 * @note Scala for Machine Learning Chapter 5 Naive Bayes Models
		 */
protected class Likelihood[T <% Double](val label: Int, val muSigma: XYTSeries, val prior: Double) {
	import Stats._, Likelihood._
  
	check(muSigma, prior)
  
		/**
		 * <p>Compute the log p(C|x of log of the conditional probability of the class given an 
		 * observation, obs and a probability density distribution.<br>
		 * The default density probability function is Normal(0, 1)</p>
		 * @param obs parameterized observation 
		 * @param density probability density function (default Gauss)
		 * @throws IllegalArgumentException if the observations are undefined
		 * @return log of the conditional probability p(C|x)
		 */
	final def score(obs: Array[T], density: Density): Double = {
		require( !obs.isEmpty, "Likelihood.score Undefined observations")
		
			// Compute the Log of sum of the likelihood and the class prior probability
			// The log likelihood is computed by adding the log of the density for each dimension.
			// Sum {log p(xi|C) }
		(obs, muSigma).zipped.foldLeft(0.0)((post, xms) => {
			val mean = xms._2._1		// mean
			val stdDev = xms._2._2	// standard deviation
			val _obs = xms._1
			val logLikelihood = density(mean, stdDev, _obs)
				
				// Avoid large value by setting a minimum value for the density probability
			post + Math.log(if(logLikelihood <  MINLOGARG) MINLOGVALUE else logLikelihood)
		}) + Math.log(prior) // Add the class likelihood p(C)
	}
	
		/**
		 * <p>DisplayUtils the content of this Likelihood class with associated labels.</p>
		 * @param labels Label of variables used to display content
		 */
	def toString(labels: Array[String]): String = {
		import org.scalaml.core.Types.ScalaMl
		
		val muSigmaStr = muSigma.map(musig => (musig._1, if(musig._2 > 0.0) musig._2 else -1.0))
			// Format the tuple muSigma= (mean, standard deviation) and the Prior 
		FormatUtils.format(muSigma, "Label\tMeans", "Standard Deviation", FormatUtils.MediumFormat, labels) + 
		FormatUtils.format(prior, "Class likelihood", FormatUtils.MediumFormat)
	}
	
	override def toString: String = toString(Array.empty)
}


		/**
		 * <p>Companion object for the Naive Bayes Likelihood class. The singleton
		 * is used to define the constructor apply for the class.</p>
		 * @author Patrick Nicolas
		 * @since March 11, 2014
		 * @note Scala for Machine Learning Chapter 5 Naive Bayes Models
		 */
object Likelihood {
	private val MINLOGARG = 1e-32
	private val MINLOGVALUE = -MINLOGARG

		/**
		 * Default constructor for he class Likelihood.
		 * @param label  Name or label of the class or prior for which the likelihood is computed.
		 * @param muSigma Array of tuples (mean, standard deviation) of the prior observations 
		 * for the model
		 * @param prior  Probability of occurrence for the class specified by the label.
		 */
	def apply[T <% Double](label: Int, muSigma: XYTSeries, prior: Double): Likelihood[T] = 
		new Likelihood[T](label, muSigma, prior)
    
	private def check(muSigma: XYTSeries, prior: Double): Unit =  {
		require( !muSigma.isEmpty, 
				"Likelihood.check Historical mean and standard deviation is undefined")
		require(prior > 0.0  && prior <= 1.0, 
				s"Likelihood.check Prior for the NB prior $prior is out of range")
	}
}


// --------------------------------  EOF --------------------------------------------------------------
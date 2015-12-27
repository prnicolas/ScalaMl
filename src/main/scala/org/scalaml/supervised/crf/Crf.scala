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
package org.scalaml.supervised.crf
	
	// Scala standard library
import scala.util.{Try, Success, Failure}
	// 3rd party frameworks or libraries

import org.apache.log4j.Logger
	// ScalaMl classes
import org.scalaml.stats.XTSeries
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.ITransform
import org.scalaml.supervised.Supervised
import org.scalaml.util.LoggingUtils
import org.scalaml.workflow.data.DataSource
import org.scalaml.libraries.crf.CrfAdapter
import CrfConfig._, LoggingUtils._, CrfAdapter._, XTSeries._


		/**
		 * Generic class for the linear chained CRF for tagging words, N-Grams or regular 
		 * expression. The class relies on the adapter classes defined in package 
		 * [[org.scalaml.libraries.crf]] which access tje IITB CRF Java libray [[iitb.CRF.*]] 
		 * 
		 * The class assumes the training sequences are loaded from file with *.raw and *.tagged 
		 * extensions. It is defined as a data transformation which model is implicitly derived
		 * from a training set {raw observations, tagged observations}
		 * 
		 * The training set of sequences is defined by the raw observations and its associated 
		 * tagged files, ''tagFiles(i).raw'' and ''tagFiles(i).tagged'' files.
		 * 
		 * The implemantation follows the standard design of supervised learning algorithm:
		 * - The classifier implements the '''ITransform''' implicit monadic data transformation
		 * - The constructor triggers the training of the classifier, making the model immutable
		 * - The classifier implements the '''Monitor''' interface to collect profile information for 
		 * debugging purpose
		 * 
		 * @constructor Create a Linear chain conditional random fields. 
		 * @throws IllegalArgumentException if nLabels, state, delimiters or taggedObs are either 
		 * undefined or out of range.
		 * @see org.scalaml.workflow.PipeOperator, iitb.Model.FeatureGenImpl, iitb.CRF.CRF
		 * @see iitb CRF library @ http://sourceforge.net/projects/crf/
		 * @param nLabels Number of labels (or tags) used in tagging training sequences of observations.
		 * @param config Minimum set of configuration parameters required to train a CRF model
		 * @param delims Delimiters used in extracting labels and data from the training files
		 * @param xt Vector of name for the pair of *.raw (observations) and *.tagged (labels) fiels
		 * (taggedIdentifier for the training data set. The training set of sequences 
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98 April 3, 2014
		 * @version 0.99.1
		 * @see Scala for Machine Learning Chapter 7 "Sequential data models" / 
		 * Conditional Random Fields.
		 * @see org.scalaml.core.ITransform
		 */
final class Crf(
		nLabels: Int, 
		config: CrfConfig, 
		delims: CrfSeqDelimiter, 
		xt: Vector[String]) extends ITransform[String](xt) with Monitor[Int] {
	import Crf._

  type V = Double
  
	check(nLabels, xt)
  
	protected val logger = Logger.getLogger("Crf")


		/*
		 * Labeled or tagged observations used for training
		 */
	private[this] val tagsGen = new CrfTagger(nLabels)
	private[this] val crf = CrfAdapter(nLabels, tagsGen, config.params)

		/*
		 * Model computes through training. The training is performed during instantiation
		 * of the class Crf. The model is set up as None if the training did not converge
		 */
	private[this] val model: Option[CrfModel] = train

		/**
		 * Predictive method for the conditional random field.
		 * @throws MatchError if the model is undefined or the input string has an incorrect size
		 * @return PartialFunction of feature of type String as input and the predicted value 
		 * of type Double as output
		 */
	override def |> : PartialFunction[String, Try[V]] = {
		case obs: String if obs.nonEmpty && model.isDefined =>
			val dataSeq =  new CrfDataSeq(nLabels, obs, delims.obsDelim)
			Try (crf.apply(dataSeq))
	}
	
		/**
		 * Return the weights or lambda values for this CRF.
		 * @return Weights of the CRF model if the model has been properly trained, None otherwise.
		 */
	final def weights: Option[DblArray] = model.map( _.weights)

		/*
		 * The training method create an iterator to traverse the tagged observations
		 * then apply IITB, CRF train method on these tagged observations. before
		 * generating the model crf.train. The method returns Try[CrfModel]
		 */
	private def train: Option[CrfModel] = Try {
		val weights = if(xt.size == 1) 
			computeWeights(xt.head)
	  
	  else {
			val weightsSeries: XVSeries[Double] = xt.map( computeWeights(_) )
			statistics(weightsSeries).map(_.mean).toArray
	  }
		new CrfModel(weights)
	}._toOption("Crf training failed", logger)
	
	
	
	private def computeWeights(tagsFile: String): DblArray = {
		val seqIter = CrfSeqIter(nLabels, tagsFile, delims)
		tagsGen.train(seqIter)
		crf.train(seqIter)
	}
}


		/**
		 * Companion object for the Linear chained Conditional random field. The singleton is used
		 * to define the constructors and validate the class parameters.
		 * @author Patrick Nicolas
		 * @since 0.98 April 3, 2014
		 * @see Scala for Machine Learning Chapter 7 Sequential data models/Conditional Random Fields.
		 */
object Crf {
	final val NUM_LABELS_LIMITS = (1, 512)
	
			/**
		 * Default constructor for the conditional random field.
		 * @param nLabels Number of labels (or tags) used in tagging training sequences of observations.
		 * @param config Minimum set of configuration parameters required to train a CRF model
		 * @param delims Delimiters used in extracting labels and data from the training files
		 * @param xs Vector of name for the pair of *.raw (observations) and *.tagged (labels) fiels
		 * (taggedIdentifier for the training data set. The training set of sequences 
		 */
	def apply(
			nLabels: Int, 
			config: CrfConfig, 
			delims: CrfSeqDelimiter, 
			xs: Vector[String]): Crf =
		new Crf(nLabels, config, delims, xs)
	
		/**
		 * Default constructor for the conditional random field.
		 * @param nLabels Number of labels (or tags) used in tagging training sequences of observations.
		 * @param config Minimum set of configuration parameters required to train a CRF model
		 * @param delims Delimiters used in extracting labels and data from the training files
		 * @param tagFile prefix fo rthe files *.raw (observations) and *.tagged (labels) fiels
		 * (taggedIdentifier for the training data set. The training set of sequences 
		 */
	def apply(nLabels: Int, config: CrfConfig, delims: CrfSeqDelimiter, tagFile: String): Crf = 
		new Crf(nLabels, config, delims, Vector[String](tagFile))
  
  
	private def check(nLabels: Int, xs: Vector[String]): Unit = {
		require(nLabels > NUM_LABELS_LIMITS._1 && nLabels < NUM_LABELS_LIMITS._2, 
				s"Number of labels for generating tags for CRF $nLabels is out of range")
		require( xs.nonEmpty, "Crf number of tag files is 0")
	}
}


// ---------------------------- EOF ------------------------------------------------------
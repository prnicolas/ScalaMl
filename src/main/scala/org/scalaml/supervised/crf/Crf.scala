/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95
 * 
 * This code uses the iitb CRF library 
 * Copyright (c) <2004> <Sunita Sarawagi Indian Institute of Technology Bombay> All rights reserved.
 */
package org.scalaml.supervised.crf

import iitb.CRF.{CRF, CrfParams, DataSequence, DataIter, FeatureGenerator}
import iitb.Model.{FeatureGenImpl, CompleteModel}
import org.scalaml.core.XTSeries
import org.scalaml.workflow.data.DataSource
import org.scalaml.core.design.{PipeOperator, Model}
import org.scalaml.supervised.Supervised
import java.io.IOException
import org.scalaml.core.types.ScalaMl._
import CrfConfig._
import Crf._
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display



	/**
	 * <p>Generic model for Conditional Random fields. The model consists merely of the CRF weights.</p>
	 * @constructor Instantiate a model for CRF after training is completed.[weights] Weights or coefficients of the CRF
	 * @throws IllegalArgumentException if weights is not properly defined
	 * @see org.scalaml.core.design.Model
	 * 
	 * @author Patrick Nicolas
	 * @since April 1, 2014
	 * @note Scala for Machine Learning Chapter 7 Sequential data models $Conditional Random Fields.
	 */
class CrfModel(val weights: DblVector) extends Model {
  require(weights != null && weights.size > 0, "CrfModel Cannot create a model with undefined weights")
  
  val persists = "models/crf"  
}

	/**
	 * <p>Generic class for the linear chained CRF for tagging words, N-Grams or regular expression. The class
	 * define a Feature generator class that inherits the default implementation FeatureGenImpl of iitb features generator.
	 * The class assumes the training sequences are loaded from file with *.raw and *.tagged extensions.
	 * The training set of sequences is defined by the raw observations and its associated tagged files, taggedObs.raw and taggedObs.tagged files.
	 * </p>
	 * @constructor Create a Linear chain conditional random fields. [nLabels]: Number of labels (or tags) used in tagging training sequences of observations, [state]: Minimum set of stateuration parameters used in the CRF, [delims]: Delimiters used in extracting labels and data from the training files, [taggedObs] identifier for the training data set. 
	 * @param nLabels Number of labels (or tags) used in tagging training sequences of observations.
	 * @param state minimum set of stateuration parameters used in the CRF
	 * @param delims delimiters used in extracting labels and data from the training files
	 * @param taggedObs identifier for the training data set. The training set of sequences is defined by the raw observations
	 * and its associated tagged file    taggedObs.raw and taggedObs.tagged files.
	 * @throws IllegalArgumentException if nLabels, state, delims and taggedObs are either undefined or out of range.
	 * @see org.scalaml.workflow.PipeOperator
	 * 
	 * @author Patrick Nicolas
	 * @since April 3, 2014
	 * @note Scala for Machine Learning
	 */
final class Crf(nLabels: Int, config: CrfConfig, delims: CrfSeqDelimiter, taggedObs: String) 
                                               extends PipeOperator[String, Double] {
	
  check(nLabels, config, delims, taggedObs)
  
  private val logger = Logger.getLogger("Crf")
  
  class TaggingGenerator(nLabels: Int) extends FeatureGenImpl(new CompleteModel(nLabels) , nLabels, true)
	
  private[this] val features = new TaggingGenerator(nLabels)
  private[this] lazy val crf = new CRF(nLabels, features, config.params)
  
  private val model: Option[CrfModel] = {
  	 val seqIter = CrfSeqIter(nLabels, taggedObs, delims)
  	 Try {
	  	 features.train(seqIter)
	  	 new CrfModel(crf.train(seqIter))
  	 } match {
  		 case Success(_model) => Some(_model)
  		 case Failure(e) => Display.error("Crf.model ", logger, e); None
  	 }
  }
  
  		/**
  		 * <p>Predictive method for the conditional random field.</p>
  		 */    
  override def |> : PartialFunction[String, Double] = {
  	 case obs: String if(obs != null && obs.length > 1 && model != None) => {
  	    val dataSeq =  new CrfTrainingSet(nLabels, obs, delims.obsDelim)
	  	crf.apply(dataSeq)
  	 }
  }
  
  final def weights: Option[DblVector] = model match {
  	case Some(m) => Some(m.weights)
  	case None => None
  }
  
  private def check(nLabels: Int, state: CrfConfig, delims: CrfSeqDelimiter, taggedObs: String): Unit = {
	 require(nLabels > NUM_LABELS_LIMITS._1 && nLabels < NUM_LABELS_LIMITS._2, "Number of labels for generating tags for CRF " + nLabels + " is out of range")
	 require(state != null, "Configuration of the linear Chain CRF is undefined")
	 require(delims != null, "delimiters used in the CRF training files are undefined")
	 require(taggedObs != null, "Tagged observations used in the CRF training files are undefined")
  }
}



	/**
	 * Companion object for the Linear chained Conditional random field. The singleton is used
	 * to define the constructors.
	 */
object Crf {
  final val NUM_LABELS_LIMITS = (1, 200)
	
  def apply(nLabels: Int, state: CrfConfig, delims: CrfSeqDelimiter, taggedObs: String): Crf = 
		           new Crf(nLabels, state, delims, taggedObs)
}


// ---------------------------- EOF ------------------------------------------------------
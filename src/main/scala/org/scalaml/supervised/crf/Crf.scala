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
 * 
 * This code uses the iitb CRF library http://sourceforge.net/projects/crf/
 * Copyright (c) <2004> <Sunita Sarawagi Indian Institute of Technology Bombay> All rights reserved.
 */
package org.scalaml.supervised.crf
	
	// Scala standard library
import scala.util.{Try, Success, Failure}
	// 3rd party frameworks or libraries
import iitb.CRF.{CRF, CrfParams, DataSequence, DataIter, FeatureGenerator}
import iitb.Model.{FeatureGenImpl, CompleteModel}
import org.apache.log4j.Logger
	// ScalaMl classes
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.Design.{PipeOperator, Model}
import org.scalaml.supervised.Supervised
import org.scalaml.util.DisplayUtils
import org.scalaml.workflow.data.DataSource
import CrfConfig._


		/**
		 * <p>Generic model for Conditional Random fields. The model consists merely of the CRF weights.</p>
		 * @constructor Instantiate a model for CRF after training is completed.
		 * @throws IllegalArgumentException if weights is not properly defined
		 * @param weights	Weights (or lambda parameters) for this CRF model.
		 * @see org.scalaml.core.Design.Model
		 * 
		 * @author Patrick Nicolas
		 * @since April 1, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models/Conditional Random Fields.
		 */
final protected class CrfModel(val weights: DblVector) extends Model {
	require(!weights.isEmpty, "CrfModel Cannot create a model with undefined weights")

		/**
		 * Constructor that load the model from file "model/CrfModel"
		 * @param className name of the class of the model, which is also the name of the file
		 * @return instance of the CrfModel with appropriate weights if the model has been 
		 * saved into file
		 * @note The client code that instantiate the model is responsible for catching the 
		 * exception if the file does not exist.
		 */
	def this(className: String) = 
			this({ Model.read(className).map( _.split(",").map(_.toDouble)).getOrElse(Array.empty) })
	
			/**
			 * Write the content of this model (weights) into a file
			 * @return true if the model/weights were saved into file, false otherwise.
			 */
	override def >> : Boolean = write(weights.mkString(","))
}

		/**
		 * <p>Generic class for the linear chained CRF for tagging words, N-Grams or regular 
		 * expression. The class define a Feature generator class that inherits the default 
		 * implementation FeatureGenImpl (feature generator defined in IITB CRF java library).<br>
		 * The class assumes the training sequences are loaded from file with *.raw and *.tagged extensions.
		 * The training set of sequences is defined by the raw observations and its associated 
		 * tagged files, taggedObs.raw and taggedObs.tagged files.</p>
		 * @constructor Create a Linear chain conditional random fields. 
		 * @throws IllegalArgumentException if nLabels, state, delimiters or taggedObs are either 
		 * undefined or out of range.
		 * @see org.scalaml.workflow.PipeOperator, iitb.Model.FeatureGenImpl, iitb.CRF.CRF
		 * @see iitb CRF library @ http://sourceforge.net/projects/crf/
		 * @param nLabels Number of labels (or tags) used in tagging training sequences of observations.
		 * @param config Minimum set of configuration parameters required to train a CRF model
		 * @param delims Delimiters used in extracting labels and data from the training files
		 * @param taggedObs Identifier for the training data set. The training set of sequences 
		 * is defined by the raw observations
		 * 
		 * @author Patrick Nicolas
		 * @since April 3, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Conditional Random Fields.
		 */
final class Crf(
		nLabels: Int, 
		config: CrfConfig, 
		delims: CrfSeqDelimiter, 
		taggedObs: String) extends PipeOperator[String, Double] {
	import Crf._
	check(nLabels)
  
	private val logger = Logger.getLogger("Crf")
 
		/**
		 * Class to generate tagged features for nLabels. The complete model is used to
		 * generate the tags.
		 */
	class TaggingGenerator(nLabels: Int) 	
			extends FeatureGenImpl(new CompleteModel(nLabels), nLabels, true)

		/*
		 * Labeled or tagged observations used for training
		 */
	private[this] val features = new TaggingGenerator(nLabels)
	private[this] val crf = new CRF(nLabels, features, config.params)

		/*
		 * Model computes through training. The training is performed during instantiation
		 * of the class Crf. The model is set up as None if the training did not converge
		 */
	private val model: Option[CrfModel] = train match {
		case Success(model) => Some(model)
		case Failure(e) => DisplayUtils.none("Crf.model could not be created", logger, e)
	}
  
		/**
		 * <p>Predictive method for the conditional random field.</p>
		 * @throws MatchError if the model is undefined or the input string has an incorrect size
		 * @return PartialFunction of feature of type String as input and the predicted value 
		 * of type Double as output
		 */
	override def |> : PartialFunction[String, Double] = {
		case obs: String if( !obs.isEmpty && model != None) => {
			val dataSeq =  new CrfTrainingSet(nLabels, obs, delims.obsDelim)
			crf.apply(dataSeq)
		}
	}
	
		/**
		 * <p>Return the weights or lambda values for this CRF.</p>
		 * @return Weights of the CRF model if the model has been properly trained, None otherwise.
		 */
	final def weights: Option[DblVector] = model.map( _.weights)

		/*
		 * The training method create an iterator to traverse the tagged observations
		 * then apply IITB, CRF train method on these tagged observations. before
		 * generating the model crf.train. The method returns Try[CrfModel]
		 */
	private def train: Try[CrfModel] = {
		val seqIter = CrfSeqIter(nLabels, taggedObs, delims)
		Try {
			features.train(seqIter)
			new CrfModel(crf.train(seqIter))
		} 
	}
}


		/**
		 * Companion object for the Linear chained Conditional random field. The singleton is used
		 * to define the constructors and validate the class parameters.
		 * @author Patrick Nicolas
		 * @since April 3, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models/Conditional Random Fields.
		 */
object Crf {
	final val NUM_LABELS_LIMITS = (1, 512)
	
		/**
		 * Default constructor for the conditional random field.
		 * @param nLabels Number of labels (or tags) used in tagging training sequences of observations.
		 * @param config Minimum set of configuration parameters required to train a CRF model
		 * @param delims Delimiters used in extracting labels and data from the training files
		 * @param taggedObs Identifier for the training data set. The training set of sequences 
		 * is defined by the raw observations
		 */
	def apply(nLabels: Int, state: CrfConfig, delims: CrfSeqDelimiter, taggedObs: String): Crf = 
		new Crf(nLabels, state, delims, taggedObs)
  
  
	private def check(nLabels: Int): Unit = {
		require(nLabels > NUM_LABELS_LIMITS._1 && nLabels < NUM_LABELS_LIMITS._2, 
				s"Number of labels for generating tags for CRF $nLabels is out of range")
	}
}


// ---------------------------- EOF ------------------------------------------------------
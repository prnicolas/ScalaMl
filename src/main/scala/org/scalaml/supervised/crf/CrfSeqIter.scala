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
 * This code uses the iitb CRF library 
 * Copyright (c) <2004> <Sunita Sarawagi Indian Institute of Technology Bombay> All rights reserved.
 */
package org.scalaml.supervised.crf

	// IITB library classes
import iitb.CRF.{CRF, CrfParams, DataSequence, DataIter}
import iitb.Model.FeatureImpl
import iitb.Segment.{DataCruncher, LabelMap}
	// ScalaMl classes
import org.scalaml.core.Types

		/**
		 * <p>Class that specifies the regular expressions used to delineates labels, observations
		 * and sequences in the training files '*.tagged'.
		 * example in training file</p>
		 * @constructor Creates a delimiters set for the raw and tagged input (training) for the CRF.
			 @throws IllegalArgumentException if any of the delimiter is undefined
		 * @param obsDelim Delimiters for observation as a sequence of N-grams or words
		 * @param labelsDelim   Delimiter between observations string and tag/label
		 * @param trainingDelim Delimiter between training sequences.
		 * @see iitb.CRF.DataSequence, iitb.CRF.DataIter, iitb.Segment.DataCruncher, iitb.Model.FeatureImpl
		 * 
		 * @author Patrick Nicolas
		 * @since April 5, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models/Conditional Random Fields.
		 */
class CrfSeqDelimiter(val obsDelim: String, val labelsDelim: String, val trainingDelim: String) {
	require(obsDelim != Types.nullString, 
			"Delimiter for observations in CRF training sequence is undefined")
	require(labelsDelim != Types.nullString, 
			"Delimiter for labels in CRF training sequence is undefined")
	require(trainingDelim != Types.nullString, 
			"Delimiter for training sequences in CRF training sequence is undefined")
}


		/**
		 * <p>Generic class that implements the iterator for sequences (training or validation). 
		 * The class needs to implement the methods of the iitb.CRF.DataIter interface. The class 
		 * delegates the generation of the training data to the iitb.Segment.DataCruncher class</p>
		 * @constructor Create a new CRF sequences iterator for nLabels labels using a given delimiter.
		 * @throws IllegalArgumentException if nLabel is out of range or the input or delim is undefined
		 * @see ittb.CRF.DataIter
		 * @param nLabels Number of labels used in the CRF model
		 * @param input Identifier for the training or tagged files
		 * @param delim  Delimiter instance used to break down the training data as sequence, 
		 * observations and labels
		 * 
		 * @author Patrick Nicolas
		 * @since April 5, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models/Conditional Random Fields.
		 */
class CrfSeqIter(val nLabels: Int, val input: String, val delim: CrfSeqDelimiter) extends DataIter {
	import CrfSeqIter._
	check(nLabels, input, delim)
	
		/**
		 * Training data set extracted from file and to be used by the CRF model
		 */
	lazy val trainData = DataCruncher.readTagged(nLabels, input, input, delim.obsDelim, 
			delim.labelsDelim, delim.trainingDelim, new LabelMap)
   
		/**
		 * Override the DataIter.hasNext interface method. Delegate the call to DataCruncher
		 */
	override def hasNext: Boolean = trainData.hasNext
     
		/**
		 * Override the DataIter.next interface method. Delegate the call to DataCruncher
		 */
	override def next: DataSequence = trainData.next
   
		/**
		 * Override the DataIter.startScan interface method. Delegate the call to DataCruncher
		 */
	override def startScan: Unit = trainData.startScan
}


	/**
	 * Companion object to SeqIter. This singleton is used to define the constructors 
	 * and validate their input parameters.
	 * @author Patrick Nicolas
	 * @since April 5, 2014
	 * @note Scala for Machine Learning Chapter 7 Sequential data models / Conditional Random Fields.
	 */
object CrfSeqIter { 
	private val MAX_NUM_LABELS = 1000
	private val DEFAULT_SEQ_DELIMITER = new CrfSeqDelimiter(",\t/ -():.;'?#`&_", "//", "\n")

		/**
		 * Default constructor for the CrfSeqIter (Sequential iterator for training data in CRF)
		 * @param nLabels Number of labels used in the CRF model
		 * @param input Identifier for the training or tagged files
		 * @param delim  Delimiter instance used to break down the training data as sequence, 
		 * observations and labels
		 */
	def apply(
			nLabels: Int, 
			input: String, 
			delim: CrfSeqDelimiter): CrfSeqIter = new CrfSeqIter(nLabels, input, delim)
	
		/**
		 * Constructor for the CrfSeqIter (Sequential iterator for training data in CRF)
		 * with a default sequential training data delimiter.
		 * @param nLabels Number of labels used in the CRF model
		 * @param input Identifier for the training or tagged files
		 */
	def apply(nLabels: Int, input: String): CrfSeqIter = 	
			new CrfSeqIter(nLabels, input, DEFAULT_SEQ_DELIMITER)
   
	private def check(nLabels: Int, input: String, delim: CrfSeqDelimiter): Unit = {
		require(nLabels > 0 && nLabels < MAX_NUM_LABELS, 
				s"CrfSeqIter.check Number of labels for the CRF model $nLabels is out of range")
		require(input != Types.nullString, 
				"CrfSeqIter.check  input for the CRF training files is undefined")
	}
}

// ---------------------------- EOF ------------------------------------------------------
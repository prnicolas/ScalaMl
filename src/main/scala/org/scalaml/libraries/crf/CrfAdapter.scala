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
package org.scalaml.libraries.crf

import iitb.CRF.{CRF, CrfParams, DataSequence, DataIter, FeatureGenerator}
import iitb.Model.{FeatureGenImpl, CompleteModel, FeatureImpl}
import iitb.Segment.{DataCruncher, LabelMap, TrainData, DCTrainRecord}



	/**
	 * Singleton adapter to the IITB CRF Java library. The object is responsible for defining
	 * the following interface classes to the IITB library
	 * 
	 * - CrfTagger Labeling or tagging utility
	 * - CrfDataSeq Generation of the sequence of observations
	 * - CrfSeqDelimiter Delimiter for a sequence of observations (or tokens)
	 * - CrfSeqIter Iterator for the sequence of observations.
	 * 
	 * @author Patrick Nicolas
	 * @version 0.99.1.1
	 * @see Scala for Machine Learning Chapter 7 ''Sequential data models'' / Conditional 
	 * Random Fields.
	 * @see iitb.CRF.*
	 */
object CrfAdapter {
	private val MAX_NUM_LABELS = 1000
	
	
	@throws(classOf[IllegalArgumentException])
	def apply(nLabels: Int, tagger: CrfTagger, config: String): CRF = {
		require(nLabels > 0 && nLabels < MAX_NUM_LABELS, 
				s"CrfAdapter.apply: Found nlabels $nLabels required > 0 and < $MAX_NUM_LABELS")
		new CRF(nLabels, tagger, config)
	}

		 
		/**
		 * Class to generate tagged features for nLabels. The complete model is used to
		 * generate the tags.
		 * @constructor Create a token tagger for the conditional random field
		 * @throws IllegalArgumentException if the arguments nLabels, entry and delim are either 
		 * undefined or out of range
		 * @param nLabels  Number of labels (or tags) used in the training of CRF
		 * 	
		 * @author Patrick Nicolas
		 * @since 0.98.2 April 2, 2014
		 * @version 0.99.1.1
		 * @see Scala for Machine Learning Chapter 7 ''Sequential data models'' / Conditional 
		 * Random Fields.
		 * @see ittb.CRF.FeatureGenImpl
		 */
	@throws(classOf[IllegalArgumentException])
	private[scalaml] class CrfTagger(nLabels: Int)
			extends FeatureGenImpl(new CompleteModel(nLabels), nLabels, true) {
	  
		require(nLabels > 0 && nLabels < MAX_NUM_LABELS, 
			s"CrfAdapter.apply: Found nlabels $nLabels required > 0 and < $MAX_NUM_LABELS")
	}
	
	
		/**
		 * Class that defines a Recommendation as a data sequence for a training set.
		 * @constructor Create a training set as a sequence of tokens for conditional random field		
		 * @throws IllegalArgumentException if the arguments nLabels, entry and delim are either 
		 * undefined or out of range
		 * @see ittb.CRF.DataSequence
		 * @param nLabels  Number of labels (or tags) used in the training of CRF
		 * @param words  Recommendation or observation as a sequence of words.
		 * @param delim  Delimiter of segments in the sequence
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98.2 April 2, 2014
		 * @version 0.99.1.1
		 * @see Scala for Machine Learning Chapter 7 ''Sequential data models'' / Conditional 
		 * Random Fields.
		 */
	@throws(classOf[IllegalArgumentException])
	private[scalaml] class CrfDataSeq(nLabels: Int, words: Vector[String], delim: String) extends DataSequence {
		check(nLabels, words)
		
		def this(nLabels: Int, taggedObs: String, delim: String) = this(nLabels, taggedObs.split(delim).toVector, delim)
		
		private[this] val map: Array[Int] = new Array[Int](nLabels)

		override def set_y(k: Int, label: Int): Unit = map(k) = label
		override def y(k: Int): Int = map(k)
		override def length: Int = words.size
		override def x(k: Int): Object = words(k)
		
		private def check(nLabels: Int, words: Vector[String]): Unit = {
			require(nLabels > 0 && nLabels < MAX_NUM_LABELS, 
				s"CrfDataSeq: Found nlabels $nLabels required > 0 and < $MAX_NUM_LABELS")
			require(words.nonEmpty, "CrfDataSeq vector of token is undefined")
	  }
	}
	
		/**
		 * Class that specifies the regular expressions used to delineates labels, observations
		 * and sequences in the training files '*.tagged'.
		 * example in training file
		 * @constructor Creates a delimiters set for the raw and tagged input (training) for the CRF.
			 @throws IllegalArgumentException if any of the delimiter is undefined
		 * @param obsDelim Delimiters for observation as a sequence of N-grams or words
		 * @param labelsDelim   Delimiter between observations string and tag/label
		 * @param seqDelim Delimiter between training sequences.
		 * @see iitb.CRF.DataSequence, iitb.CRF.DataIter, iitb.Segment.DataCruncher and  
		 * iitb.Model.FeatureImpl
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98.2 April 5, 2014
		 * @see Scala for Machine Learning Chapter 7 ''Sequential data models'' / Conditional 
		 * Random Fields.
		 */
	@throws(classOf[IllegalArgumentException])
	private[scalaml] case class CrfSeqDelimiter(obsDelim: String, labelsDelim: String, seqDelim: String) {
		require( !obsDelim.isEmpty, 
			"Delimiter for observations record is undefined")
		require( !labelsDelim.isEmpty, 
			"Delimiter for labeled or tagged record is undefined")
		require( !seqDelim.isEmpty,
			"Delimiter for data sequence is undefined")
	}

			
		/**
		 * Generic class that implements the iterator for sequences (training or validation). 
		 * The class needs to implement the methods of the iitb.CRF.DataIter interface. The class 
		 * delegates the generation of the training data to the iitb.Segment.DataCruncher class
		 * @constructor Create a new CRF sequences iterator for nLabels labels using a given delimiter.
		 * @throws IllegalArgumentException if nLabel is out of range or the input or delim is undefined
		 * @see ittb.CRF.DataIter
		 * @param nLabels Number of labels used in the CRF model
		 * @param input Identifier for the training or tagged files
		 * @param delim  Delimiter instance used to break down the training data as sequence, 
		 * observations and labels
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98.2 April 5, 2014
		 * @see Scala for Machine Learning Chapter 7 ''Sequential data models'' / Conditional 
		 * Random Fields.
		 */
	@throws(classOf[IllegalArgumentException])
	private[scalaml] class CrfSeqIter(nLabels: Int, input: String, delim: CrfSeqDelimiter) extends DataIter {
		import CrfSeqIter._
		check(nLabels, input)
	
		/**
		 * Training data set extracted from file and to be used by the CRF model
		 */
		
		lazy val trainData: TrainData = DataCruncher.readTagged(nLabels, input, input, delim.obsDelim, 
			delim.labelsDelim, delim.seqDelim, new LabelMap)
   
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
		override def startScan(): Unit = trainData.startScan()
	}	


	/**
	 * Companion object to SeqIter. This singleton is used to define the constructors 
	 * and validate their input parameters.
	 * @author Patrick Nicolas
	 * @since 0.98.2 April 5, 2014
	 * @see Scala for Machine Learning Chapter 7 ''Sequential data models'' / Conditional 
	 * Random Fields.
	 */
	object CrfSeqIter { 
		private val DEFAULT_SEQ_DELIMITER = new CrfSeqDelimiter(",\t/ -():.;'?#`&_", "//", "\n")

		/**
		 * Default constructor for the CrfSeqIter (Sequential iterator for training data in CRF)
		 * @param nLabels Number of labels used in the CRF model
		 * @param input Identifier for the training or tagged files
		 * @param delim  Delimiter instance used to break down the training data as sequence, 
		 * observations and labels
		 */
		@throws(classOf[IllegalArgumentException])
		def apply(
				nLabels: Int, 
				input: String, 
				delim: CrfSeqDelimiter): CrfSeqIter = {
		  
			check(nLabels, input)
			new CrfSeqIter(nLabels, input, delim)
		}
	
		/**
		 * Constructor for the CrfSeqIter (Sequential iterator for training data in CRF)
		 * with a default sequential training data delimiter.
		 * @param nLabels Number of labels used in the CRF model
		 * @param input Identifier for the training or tagged files
		 */
		@throws(classOf[IllegalArgumentException])
		def apply(nLabels: Int, input: String): CrfSeqIter = 	
				new CrfSeqIter(nLabels, input, DEFAULT_SEQ_DELIMITER)
   
		private def check(nLabels: Int, input: String): Unit = {
			require(nLabels > 0 && nLabels < MAX_NUM_LABELS, 
				s"CrfAdapter.apply: Found nlabels $nLabels required > 0 and < $MAX_NUM_LABELS")
			require( !input.isEmpty, 
				"CrfSeqIter.check  input for the CRF training files is undefined")
		}
	}
}


// ------------------------------  EOF --------------------
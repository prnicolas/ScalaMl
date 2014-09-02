/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * This code uses the iitb CRF library 
 * Copyright (c) <2004> <Sunita Sarawagi Indian Institute of Technology Bombay> All rights reserved.
 */
package org.scalaml.supervised.crf

import iitb.CRF.{CRF, CrfParams, DataSequence, DataIter}
import java.util.Properties
import iitb.Model.FeatureImpl
import iitb.Segment.{DataCruncher, LabelMap}


	/**
	 * <p>Class that defines a Recommendation as a data sequence.</p>
	 * @param nLabels Number of labels (or tags) used in the training of CRF
	 * @param entry recommendation or observation as a sequence of words
	 * @param delim delimiter of segments in the sequence
	 * @throws IllegalArgumentException if the arguments nLabels, entry and delim are either undefined or out of range
	 * @see ittb.CRF.DataSequence
	 * @author Patrick Nicolas
	 * @since April 2, 2014
	 */
class CrfRecommendation(val nLabels: Int, val entry: String, val delim: String) extends DataSequence {
    private val words: Array[String] = entry.split(delim)
    private val map: Array[Int] = new Array[Int](nLabels)
   
    override def set_y(k: Int, label: Int): Unit = map(k) = label
    override def y(k: Int): Int = map(k)
    override def length: Int = words.size
    override def x(k: Int): Object = words(k)
}


	/**
	 * <p>Class that specifies the regular expressions used to delineates labels, observations
	 * and sequences in the training files '*.tagged'.
	 * example in training file<br>
	 * word1 [dObs] word2 [dObs]... wordn [dLabel] label<br>
	 * [dSeq]<br>
	 * </p>
	 * @param dObs delimiters for observation as a sequence of N-grams or words
	 * @param dLabel delimiter between observations string and tag/label
	 * @param dSeq delimiter between training sequences
	 * @throws IllegalArgumentException if dObs, dLabel or dSeq are null (undefined)
	 * @author Patrick Nicolas
	 * @since April 5, 2014
	 */
case class CrfSeqDelimiter(val dObs: String, val dLabel: String, val dSeq:String) {
	require(dObs != null, "Delimiter for observations in CRF training sequence is undefined")
	require(dLabel != null, "Delimiter for labels in CRF training sequence is undefined")
	require(dSeq != null, "Delimiter for training sequences in CRF training sequence is undefined")
}


	/**
	 * <p>Generic class that implements the iterator for sequences (training or validation). The class needs
	 * to implement the methods of the iitb.CRF.DataIter interface. The class delegates the generation of 
	 * the training data to the iitb.Segment.DataCruncher class</p>
	 * @param nLabels number of labels used in the CRF  model
	 * @param input identifier for the training or tagged files
	 * @param delim delimiter instances used to break down the training data as sequence, observations and labels
	 * @throws IllegalArgumentException if nLabel is out of range or the input or delim is undefined
	 * @throws IOException if the training file '*.tagged' is not found
	 * @author Patrick Nicolas
	 * @since April 5, 2014
	 */
class CrfSeqIter(val nLabels: Int, val input: String, val delim: CrfSeqDelimiter) extends DataIter {
   require(nLabels > 0 && nLabels < 1000, "Number of labels for the CRF model " + nLabels + " is out of range")
   require(input != null, "Identifier for the CRF training files is undefined")
   require(delim != null, "Delimiter for the CRF training files is undefined")
	
   	/**
   	 * Training data set extracted from file and to be used by the CRF model
   	 */
   lazy val trainData =  DataCruncher.readTagged(nLabels, input, input, delim.dObs, delim.dLabel, delim.dSeq, new LabelMap)
   
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
	 * Companion object to SeqIter
	 * @author Patrick Nicolas
	 */
object CrfSeqIter { 
   final val DEFAULT_SEQ_DELIMITER = CrfSeqDelimiter(",\t/ -():.;'?#`&_", "//", "\n")
   def apply(nLabels: Int, input: String, delim: CrfSeqDelimiter): CrfSeqIter = new CrfSeqIter(nLabels, input, delim)
   def apply(nLabels: Int, input: String): CrfSeqIter = new CrfSeqIter(nLabels, input, DEFAULT_SEQ_DELIMITER)
}



// ---------------------------- EOF ------------------------------------------------------
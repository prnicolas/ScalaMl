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

import iitb.CRF.{CRF, CrfParams, DataSequence, DataIter}
import java.util.Properties
import iitb.Model.FeatureImpl
import iitb.Segment.{DataCruncher, LabelMap}


		/**
		 * <p>Class that defines a Recommendation as a data sequence for a training set.</p>
		 * @constructor Create a training set for a CRF. 		
		 * @throws IllegalArgumentException if the arguments nLabels, entry and delim are either 
		 * undefined or out of range
		 * @see ittb.CRF.DataSequence
		 * @param nLabels  Number of labels (or tags) used in the training of CRF
		 * @param entry  Recommendation or observation as a sequence of words.
		 * @param delim  Delimiter of segments in the sequence
		 * 
		 * @author Patrick Nicolas
		 * @since April 2, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models/Conditional Random Fields.
		 */
class CrfTrainingSet(val nLabels: Int, val entry: String, val delim: String) extends DataSequence {
	import CrfTrainingSet._
	
	check(nLabels, entry, delim)
	
	private[this] val words: Array[String] = entry.split(delim)
	private[this] val map: Array[Int] = new Array[Int](nLabels)
   
	override def set_y(k: Int, label: Int): Unit = map(k) = label
	override def y(k: Int): Int = map(k)
	override def length: Int = words.size
	override def x(k: Int): Object = words(k)
}

		/**
		 * Companion object to the training set for the Conditional Random field. This 
		 * singleton is used to validate the parameters of the class.
		 * @author Patrick Nicolas
		 * @since April 2, 2014
		 * @note Scala for Machine Learning Chapter 7 Sequential data models/Conditional Random Fields.
		 */
object CrfTrainingSet {
	import Crf._
	
	private def check(nLabels: Int, entry: String, delim: String): Unit = {
		require(nLabels >= NUM_LABELS_LIMITS._1 && nLabels < NUM_LABELS_LIMITS._2)
	}
}


// ---------------------------- EOF ------------------------------------------------------
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
 * Version 0.99
 */
package org.scalaml.supervised.bayes.text

import scala.collection._
import scala.util.Try

import org.apache.log4j.Logger
import org.scalaml.core.ETransform
import org.scalaml.workflow.data.DocumentsSource
import org.scalaml.util.DisplayUtils
import org.scalaml.util.MapUtils.Counter
import DocumentsSource._, TextAnalyzer._
import org.scalaml.workflow.data.Document
import org.scalaml.core.Types.ScalaMl.{XVSeries, DblArray}



		/**
		 * Simple text analyzer that extract the relative frequencies of keywords defined in a
		 * lexicon. The text analyzer is implemented as a data transformation using an explicit
		 * configuration (Lexicon) '''ETransform'''
		 * {{{
		 *   1. Parse content of each document
		 *   2. Counts the number of occurrences for a select set of keywords from each document
		 *   3. Aggregate the count of occurrences of keywords  
		 *   4. Compute the relative frequence for the set of keywords
		 * }}}
		 * @tparam T type of date or time stamp of documents
		 * @constructor Create a text analyzer with a given parser and lexicon
		 * @param parser Basic document/text parser
		 * @param lexicon semantic or synonyms map
		 * @author Patrick Nicolas
		 * @since 0.99  June 17, 2015
		 * @see Scala for Machine Learning Chapter 5 "Naive Bayes Models" / Naive Bayes and text mining
		 * @see org.scalaml.core.ETransform
		 */
class TextAnalyzer[T <: AnyVal](
   parser: TextParser, 
   lexicon: Lexicon)(implicit f: T => Long) extends ETransform[Lexicon](lexicon) {
  
  	/**
  	 * Input type U for the ETransform, Corpus
  	 */
  type U = Corpus[T]
  		/**
  		 * Output type V for the ETransform, sequence of map of relative frequencies terms.
  		 */
  type V = Seq[TermsRF]
  
  override def |> : PartialFunction[U, Try[V]] = {
    case docs: U => Try( score(docs) )
  }
  
  
  private def score(corpus: Corpus[T]): Seq[TermsRF] = {
				
				// Count the occurrences of news article for each specific date
		val termsCount: Seq[(T, Counter[String])] = corpus.map(doc => (doc.date, count(doc.content)) )
			
	//	val allTermsCount = termsCount.map{ case (t,n) => n}
	//				.aggregate(new Counter[String])((s, cnt) => s ++ cnt, _ ++ _)
						  
				// Aggregate the term count for all documents related to each date
	  val termsCountMap: Map[T, Counter[String]] = 
	         termsCount.groupBy( _._1).map { case (t, seq) =>   
	    (t, seq.aggregate(new Counter[String])((s, cnt) => s ++ cnt._2, _ ++ _) )  
	  }
					
				// Sort and reformat the term counts per date
	  val termsCountPerDate = termsCountMap.toSeq.sortWith( _._1 < _._1).unzip._2
	  
	  		// Compute the terms count for the entire corpus
	  val allTermsCount = termsCountPerDate.aggregate(new Counter[String])((s, cnt) => s ++ cnt, _ ++ _)
	  		// Computes the relative (normalized) frequencies of each terms.
	  termsCountPerDate.map( _ /allTermsCount).map(_.toMap)
	}
  
  def quantize(termsRFSeq: Seq[TermsRF]): Try[(Array[String], XVSeries[Double])] = Try {
    val keywords: Array[String] = lexicon.values.toArray.distinct
    
    val features: Seq[DblArray] = termsRFSeq.map( tf =>  
      keywords.map(key => if( tf.contains(key)) tf.get(key).get else 0.0) )
    (keywords, features.toVector)
  }
  
    	/**
  	 * Count the number of occurrences of a term. The function toWords
  	 * extract the keywords matching the lexicon from a string or file entry.
  	 */
	private def count(term: String): Counter[String] = {
		require( term.length > 0, 
				"TermsScore.count: Cannot count the number of words in undefined text")

		parser(term)./:(new Counter[String])((cnt, w) => 
			if( lexicon.contains(w)) cnt + lexicon(w) else cnt )
	}
}


object TextAnalyzer {
  type TermsRF = Map[String, Double]
  type TextParser = String => Array[String]
  type Lexicon = immutable.Map[String, String]
  
  def apply[T <: AnyVal]( 
    parser: TextParser, 
    lexicon: Lexicon)(implicit f: T => Long): TextAnalyzer[T] = new TextAnalyzer[T](parser,lexicon)
  
}
	
// ----------------------------  EOF ------------------------------------------
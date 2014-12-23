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
package org.scalaml.supervised.bayes.text

import org.scalaml.core.Types
import org.scalaml.workflow.data.DocumentsSource
import org.scalaml.util.MapUtils.Counter
import scala.annotation.implicitNotFound
import scala.collection._
import scala.util.{Try, Success, Failure}
import org.scalaml.util.DisplayUtils
import org.apache.log4j.Logger

import TermsScore._, DocumentsSource._

		/**
		 * <p>Class to extract and score terms extracted from a set of news articles.></p>
		 * @constructor Instantiates a terms extractor and scoring class. 
		 * @see org.scalaml.util.MapUtils.Counter
		 * @throws IllegalArgumentException if one of the class parameters is undefined
		 * @throws ImplicitNotFoundException if ordering is not defined prior the instantiation 
		 * of this class
		 * @param toDate  Function to convert a string date into a Long.
		 * @param toWords  Function to extracts an array of a keywords from a line.
		 * @param lexicon  Simple dictionary or map of tuples (words, stem word)
		 * 
		 * @author Patrick Nicolas
		 * @since April 6, 2014
		 * @note Scala for Machine Learning  Chapter 5 Naive Bayes Models / Naive Bayes and text mining
		 */
@implicitNotFound("Ordering not explicitly defined for NaiveBayesTextScoring class")
final class TermsScore[T <% Long](	
		toDate: String =>T, 
		toWords: String => Array[String], 
		lexicon: immutable.Map[String, String])(implicit order: Ordering[T]) {
	
  import TermsScore._, NewsArticles._

	check(lexicon)
	private val logger = Logger.getLogger("TermsScore")
   
		/**
		 * <p>Method to organize a corpus (set of documents) into a ordered sequence of map of
		 * New articles tuples (data, weighted terms).</p>
		 * @param corpus Corpus of news articles or documents
		 * @throws IllegalArgumentException if the corpus is not defined
		 * @return news articles (sequence of (date, weighted terms) map, ordered by date of release
		 */
	def score(corpus: Corpus): Option[NewsArticles[T]] = {
		Try {
			val docs = rank(corpus)
				// Count the occurrences of news article for each specific date
			val cnts = docs.map(doc => (doc._1, count(doc._3)) )
			
				// Total number of occurrences 
			val totalCnts = cnts.map( _._2)
												.foldLeft(new Counter[String])((s, cnt) => s ++ cnt) 
												
				// Initialize and populate the mutable map of news articles
			val articles = NewsArticles[T]
			cnts.foreach(cnt => articles += (cnt._1, (cnt._2/totalCnts).toMap))
			articles
		} 
		match {
			case Success(newsarticles) => Some(newsarticles)
			case Failure(e) => DisplayUtils.none("TermsScore.score ", logger, e)
		}
	}

  	/**
  	 * Count the number of occurrences of a term. The function toWords
  	 * extract the keywords matching the lexicon from a string or file entry.
  	 */
	private[this] def count(term: String): Counter[String] = {
		require(term != Types.nullString, 
				"TermsScore.count: Cannot count the number of words in undefined text")

		toWords(term).foldLeft(new Counter[String])((cnt, w) => 
			if( lexicon.contains(w)) cnt + lexicon(w) else cnt )
	}

  	/**
  	 * Rank the document within this corpus per their date
  	 * @param corpus Group of documents or news articles.
  	 */
	private[this] def rank(corpus: Corpus): CorpusType[T] = {
		require( !corpus.isEmpty, 
				"TermsScore.rank: Cannot order an undefined corpus of document")
		corpus.map(doc => (toDate(doc._1.trim), doc._2, doc._3)).sortWith( _._1 < _._1)
	}
}
   

		/**
		 * <p>Companion object for TermsScore to define constructors, validate their input
		 * parameters and define data type for corpus, <b>CorpusType</b>.</>
		 * @author Patrick Nicolas
		 * @since April 6, 2014
		 * @note Scala for Machine Learning  Chapter 5 Naive Bayes Models / Naive Bayes and text mining
		 */
object TermsScore {
		/**
		 * <p>Define the type for a corpus as a array of tuples (date, document title, document 
		 * content). The type for date is bounded (view) to a Long (convertible to a Long).</p>
		 */
	type CorpusType[T] = Array[(T, String, String)]
	
		/** 
		 * Default constructor for the class TermsScore
		 * @param toDate  Function to convert a string date into a Long.
		 * @param toWords  Function to extracts an array of a keywords from a line.
		 * @param lexicon  Simple dictionary or map of tuples (words, stem word)
		 */
	def apply[T <% Long](
			tStamp: 	String =>T, 
			words: 		String => Array[String], 
			lexicon:  immutable.Map[String, String])(implicit ordering: Ordering[T]): TermsScore[T] = 
		new TermsScore[T](tStamp, words, lexicon)
 
		
	private def check(lexicon: immutable.Map[String, String]): Unit = 
		require( !lexicon.isEmpty, "TermsScore.check Cannot score a text without a lexicon")
}

// ----------------------------  EOF ------------------------------------------
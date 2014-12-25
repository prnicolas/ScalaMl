/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.app.chap5

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.workflow.data.{DataSource,DocumentsSource}
import org.scalaml.filtering.SimpleMovingAverage
import org.scalaml.util.DisplayUtils
import org.scalaml.stats.Stats
import org.scalaml.supervised.bayes.NaiveBayes
import org.scalaml.supervised.bayes.text.{TermsScore, NewsArticles}
import org.scalaml.app.Eval


		/**
		 * <p><b>Purpose:</b> Singleton to evaluate a text retrieval application using the 
		 * Naive Bayes algorithm. The test consists of:<br>
		 * <ul>
		 * 	<li>Collecting and organizing news articles regarding a specific stock</li>
		 *  <li>Extracting and scoring the essential keywords</li>
		 *  <li>Evaluating which keyword has the most impact on the stock price using Naive Bayes</li>
		 * </ul>
		 * 
		 * @author Patrick Nicolas
		 * @since May 13, 2014
		 * @note Scala for Machine Learning Chapter 5 Naive Bayes Model
		 */
object TextBayesEval extends Eval {
	import java.text.DecimalFormat
	import scala.util.{Try, Success, Failure}
	import scala.io.Source
	import scala.collection._
	import org.apache.log4j.Logger
	import ScalaMl._, DocumentsSource._, SimpleMovingAverage._
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "TextBayesEval"
	
	private val pathCorpus = "resources/text/chap5/"
	private val pathLexicon = "resources/text/lexicon.txt"
	
		// Convert a date into a Long value so the news articles can 
		// be ordered and the weighted terms frequencies be aggregated for each date.
	private def toDate(date: String): Long = {
		val idx1 = date.indexOf(".")
		val idx2 = date.lastIndexOf(".")
		if( idx1 != -1 && idx2 != -1) 
			(date.substring(0, idx1) + date.substring(idx1+1, idx2)).toLong 
		else -1
	}

		
		 // Map of keywords which associates keywords with a semantic equivalent (poor man's stemming
		 // and lemmatization), loaded from a dictionary/lexicon file 
	private val LEXICON = loadLexicon

		
		 // Regular expression to extract keywords and match them against the lexicon 
	def toWords(content: String): Array[String] = {	
		val regExpr = "['|,|.|?|!|:|\"]"
		content.trim.toLowerCase
					.replace(regExpr," ")
					.split(" ")
					.filter( _.length > 2)
	}
    
		// Stock prices for TSLA indexed by their date from the oldest to the newest.	 
	private val TSLA_QUOTES = Array[Double](
		250.56, 254.84, 252.66, 252.94, 253.21, 255.84, 234.41, 241.49, 237.79, 230.97, 
		233.98, 240.04, 235.84, 234.91, 228.89, 220.17, 220.44, 212.96, 207.32, 212.37, 
		208.45, 216.97, 230.29, 225.4, 212.22, 207.52, 215.46, 203.93, 204.19, 197.78, 
		198.09, 201.91, 199.11, 198.12, 197.38, 205.64, 207.99, 209.86, 199.85 )
        

		/**
		 * <p>Execution of the scalatest for <b>NaiveBayes</b> class for text mining application.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int  = {
		DisplayUtils.show(s"$header $name: Multinomial Naive Bayes for text analysis", logger)
    	
		val corpus: Corpus = DocumentsSource(pathCorpus) |>
		val ts = new TermsScore[Long](toDate, toWords, LEXICON)
		Try {
			ts.score(corpus) match {
				case Some(terms) => {    // computes the difference in price between current 
					var prevQ = 0.0       // trading session and the previous session.      
					val diff = TSLA_QUOTES.map(q => {
						val delta = if(q > prevQ) 1 else 0
						prevQ = q
						delta
					})

					//Extracts the unique column names as the lemme in the Lexicon.
					val columns = LEXICON.values
										.foldLeft(new mutable.HashSet[String])((hs, key) => {hs.add(key); hs})
										.toArray	

					// Computes the relative frequencies of the lemmed terms zipped with
					// the direction of the stock movement between two consecutive trading sessions.
					val relFreQ = terms.toOrderedArray
										.zip(diff)
										.map(x => (x._1._2, x._2))
										.map(lbl => (columns.map(f => 
												if(lbl._1.contains(f)) lbl._1(f) else 0.0), lbl._2) )

					val xt = XTSeries[(DblVector, Int)](relFreQ)
					val nb = NaiveBayes[Double](xt)

					// DisplayUtils the pairs (mean, standard deviation) for each term.
					val labels: Array[String] = columns.map( _.toString).toArray
					DisplayUtils.show(s"$name Naive Bayes text extraction model${nb.toString(labels)}", 
							logger)
				}
				case None => DisplayUtils.error(s"$name keywords extraction failed", logger)
			}
		}
		match {
			case Success(n) => n
			case Failure(e) => failureHandler(e)
		}	
	}


	private def loadLexicon: immutable.Map[String, String] = {
		val src = Source.fromFile(pathLexicon)
		val fields = src.getLines.map( _.split(",").map(_.trim))
		val lexicon = fields.foldLeft(new mutable.HashMap[String, String])((hm, field) => {
			hm.put(field(0), field(1))
			hm
		}).toMap
		
		src.close
		lexicon
	}
}

// -----------------------------  EOF ------------------------------------
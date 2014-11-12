/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95d
 */
package org.scalaml.app.chap5


import org.scalaml.supervised.bayes._
import org.scalaml.core.{types, XTSeries}
import org.scalaml.workflow.data.{DataSource,DocumentsSource}
import scala.collection.mutable.ArrayBuffer
import types.ScalaMl._
import org.scalaml.filtering.SimpleMovingAverage
import SimpleMovingAverage._
import scala.collection.mutable.HashSet
import org.scalaml.supervised.bayes.NaiveBayes
import org.scalaml.util.Display
import org.apache.log4j.Logger
import org.scalaml.stats.Stats
import org.scalaml.supervised.bayes.text.TermsScore
import DocumentsSource._
import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import org.scalaml.supervised.bayes.text.NewsArticles
import java.text.DecimalFormat
import scala.io.Source
import scala.collection.mutable.HashMap
import org.scalaml.app.Eval



		/**
		 * <p>Text retrieval application of the Naive Bayes algorithm. The test consists of:<br>
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
	val name: String = "TextBayesEval"
	final val pathCorpus = "resources/text/chap5/"
    final val pathLexicon = "resources/text/lexicon.txt"
    	
	private val logger = Logger.getLogger(name)
	
		// Convert a date into a Long value so the news articles can 
	    // be ordered and the weighted terms frequencies be aggregated for each date.
	private def toDate(date: String): Long = {
		val idx1 = date.indexOf(".")
		val idx2 = date.lastIndexOf(".")
		if( idx1 != -1 && idx2 != -1) (date.substring(0, idx1) + date.substring(idx1+1, idx2)).toLong else -1
	}

		
		 // Map of keywords which associates keywords with a semantic equivalent (poor man's stemming
		 // and lemmatization), loaded from a dictionary/lexicon file 
	 val LEXICON = loadLexicon

		
		 // Regular expression to extract keywords and match them against the lexicon 
	def toWords(content: String): Array[String] = {
    	val regExpr = "['|,|.|?|!|:|\"]"
    	content.trim.toLowerCase.replace(regExpr," ").split(" ").filter( _.length > 2)
    }
    
    	 // Stock prices for TSLA indexed by their date from the oldest to the newest.	 
     final val TSLA_QUOTES = Array[Double](
    	250.56, 254.84, 252.66, 252.94, 253.21, 255.84, 234.41, 241.49, 237.79, 230.97, 233.98, 240.04, 235.84,
        234.91, 228.89, 220.17, 220.44, 212.96, 207.32, 212.37, 208.45, 216.97, 230.29, 225.4, 212.22, 207.52,
        215.46, 203.93, 204.19, 197.78, 198.09, 201.91, 199.11, 198.12, 197.38, 205.64, 207.99, 209.86, 199.85 )
        
	def run(args: Array[String]): Int  = {
    	Display.show("TextBayesEval: Evaluation Multinomial Naive Bayes for text analysis", logger)
    	
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
			  	                        .foldLeft(new HashSet[String])((hs, key) => {hs.add(key); hs})
			  	                        .toArray	
			  	   
			  	       // Computes the relative frequencies of the lemmed terms zipped with
			  	       // the direction of the stock movement between two consecutive trading sessions.
				   val relFreQ = terms.toOrderedArray
			  	  	                  .zip(diff)
			  	  	                  .map(x => (x._1._2, x._2))
			  	  	                  .map(lbl => (columns.map(f => if(lbl._1.contains(f)) lbl._1(f) else 0.0), lbl._2) )
			  	  	                      
				   val xt = XTSeries[(DblVector, Int)](relFreQ)
			  	   val nb = NaiveBayes[Double](xt)
			  	   
			  	   	  // Display the pairs (mean, standard deviation) for each term.
			  	   displayResults(columns, nb.toString)
			  	}
			  	case None => Display.error("TextBayesEval keywords extraction failed", logger)
			 }
	    }match {
	    	case Success(n) => n
	    	case Failure(e) => Display.error("TextBayesEval.run", logger, e)
	    }
	}
    
    private def displayResults(columns: Array[String], results: String): Int = {
        val buf = new StringBuilder
        buf.append("Keywords:\t")
        columns.foreach( c => buf.append(Display.align(c.toString, 14)))
        Display.show(s"TextBayesEval: text extraction model\n ${buf.toString}${results}", logger)
    }
    
    private def loadLexicon: Map[String, String] = {
    	 val src = Source.fromFile(pathLexicon)
	  	 val fields = src.getLines.map( _.split(",").map(_.trim))
         fields.foldLeft(new HashMap[String, String])((hm, field)=> {hm.put(field(0), field(1)); hm}).toMap
    }
}

// -----------------------------  EOF ------------------------------------
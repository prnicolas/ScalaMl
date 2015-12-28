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
package org.scalaml.app.chap5

import scala.language.implicitConversions

import org.scalaml.stats.{Stats, XTSeries, Difference}
import org.scalaml.core.Types.ScalaMl.{DblVector, DblArray}
import org.scalaml.workflow.data.{DataSource,DocumentsSource}
import org.scalaml.filtering.movaverage.SimpleMovingAverage
import org.scalaml.util.{DisplayUtils,  LoggingUtils}
import org.scalaml.supervised.bayes.NaiveBayes
import org.scalaml.supervised.bayes.text.TextAnalyzer
import org.scalaml.app.Eval
import LoggingUtils._, XTSeries._, Difference._



		/**
		 * '''Purpose:''' Singleton to evaluate a text retrieval application using the 
		 * Naive Bayes algorithm. 
		 * 
		 * This program attempts to find a correlation between financial news regarding a company
		 * and the price behavior of its stock one or more days later
		 * 
		 * {{{ 
		 *  The test consists of:
		 * 	 - Collecting and organizing news articles regarding a specific stock
		 *   - Extracting and scoring the essential keywords
		 *   - Evaluating which keyword has the most impact on the stock price using Naive Bayes
		 * }}}
		 * 
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see org.scalaml.supervised.bayes.text
		 * @see Scala for Machine Learning Chapter 5 ''Naive Bayes Model''
		 */
object TextBayesEval extends Eval {
	import java.text.{SimpleDateFormat, DecimalFormat}
	import scala.util.Try
	import scala.io.Source
	import scala.collection._
	import org.apache.log4j.Logger
	import DocumentsSource._, SimpleMovingAverage._
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "TextBayesEval"
	
	private val pathCorpus = "resources/text/chap5/"
	private val pathLexicon = "resources/text/lexicon.txt"
	  
	private val dateFormat: SimpleDateFormat = new SimpleDateFormat("MM.dd.yyyy")


		
		 // Map of keywords which associates keywords with a semantic equivalent (poor man's stemming
		 // and lemmatization), loaded from a dictionary/lexicon file 
	private val LEXICON = loadLexicon

		
		 // Regular expression to extract keywords and match them against the lexicon 
	def parse(content: String): Array[String] = {	
		val regExpr = "['|,|.|?|!|:|\"]"
		content.trim.toLowerCase
					.replace(regExpr," ")
					.split(" ")
					.filter( _.length > 2)
	}
    
		// Stock prices for TSLA indexed by their date from the oldest to the newest.	 
	private val TSLA_QUOTES = Vector[Double](
		250.56, 254.84, 252.66, 252.94, 253.21, 255.84, 234.41, 241.49, 237.79, 230.97, 
		233.98, 240.04, 235.84, 234.91, 228.89, 220.17, 220.44, 212.96, 207.32, 212.37, 
		208.45, 216.97, 230.29, 225.4, 212.22, 207.52, 215.46, 203.93, 204.19, 197.78, 
		198.09, 201.91, 199.11, 198.12, 197.38, 205.64, 207.99, 209.86, 199.85 )

	  
		/**
		 * Execution of the scalatest for '''NaiveBayes''' class for text mining application.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate.
		 * 	
		 * Exceptions thrown during the execution of the tests are caught by the wrapper or handler
		 * test method in Eval trait defined as follows:
		 * {{{
		 *    def test(args: Array[String]) =
		 *      Try(run(args)) match {
		 *        case Success(n) => ...
		 *        case Failure(e) => ...
		 * }}}
		 * The tests can be executed through ''sbt run'' or individually by calling 
		 * ''TestName.test(args)'' (i.e. DKalmanEval.test(Array[String]("IBM") )
		 * @param args array of arguments used in the test
		 */
	override protected def run(args: Array[String]): Int = {
		import scala.language.postfixOps
		show(s"$header Multinomial Naive Bayes for text analysis")
	  

			// Generate the partial function to extract documents from a directory or set of files
		val pfnDocs = DocumentsSource(dateFormat, pathCorpus) |>
		
			// Create a text analyzer with a time stamp of type Long, a parsing function and a 
			// given Lexicon
		val textAnalyzer = TextAnalyzer[Long](parse, LEXICON)
	  
			// Partial function for the text analyzer
		val pfnText = textAnalyzer |>

		
		(for {
				// Extract the corpus from the documents
			corpus <- pfnDocs(None)
			
				// Extract the terms frequencies map from the corpus
			if pfnText.isDefinedAt(corpus)
			termsFreq <- pfnText(corpus)
			
				// Create the features set from quantization
			featuresSet <- textAnalyzer.quantize(termsFreq)
			
				// Extract the labels by computing the difference of the stock prices
				// between trading sessions
			expected <- Try { difference(TSLA_QUOTES, diffInt) } 
			
				// Generate the classification model based on Naive-Bayes
			nb <- NaiveBayes[Double](1.0, featuresSet._2.zip(expected))
		} 
		yield {
			val likelihoods = nb.model.get.likelihoods
			val means: Seq[DblVector] = likelihoods.map( _.muSigma.map(_._1))
			display(means, "Naive Bayes text classification", Set[String]("Positives", "Negatives"))
			
			val results = s"""Corpus ----- \n${corpus.mkString("\n")}
				| Terms frequencies ----- \n
				| ${termsFreq.map( _.map(e => s"(${e._1},${e._2})").mkString(" | ")).mkString("\n")}
				| Expected ----- \n${expected.mkString("\n")}
				| Quantized ----- \n${featuresSet._1.mkString(",")}
				| \n${featuresSet._2.map( _.mkString(",")).mkString("\n")}
				| \nText extraction model${nb.toString(featuresSet._1)}""".stripMargin
		  show(results)
		}).get
		
		
	}

		/**
		 * Extract the lexicon (HashMap) from a file.
		 */
	private def loadLexicon: immutable.Map[String, String] = {
		val src = Source.fromFile(pathLexicon)
		val fields: Array[Array[String]] = src.getLines.map( _.split(",").map(_.trim)).toArray
		val lexicon = fields./:(new mutable.HashMap[String, String])((hm, field) => 
			hm += ((field(0), field(1)))
		).toMap
		
		src.close
		lexicon
	}
	
	private def display(values: Seq[DblVector], label: String, labels: Set[String]): Unit = {
		import org.scalaml.plots.{LinePlot, LightPlotTheme, Legend}
		
		val legend = Legend( 
			name, label, "Positive/negative outcome", "Mean values"
		)
		
		val dataPoints = values.map(_.toVector).zip(labels)
		LinePlot.display(dataPoints.toList, legend, new LightPlotTheme)
	}
}


// -----------------------------  EOF ------------------------------------
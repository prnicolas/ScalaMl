/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.app.chap5


import org.scalaml.supervised.bayes._
import org.scalaml.core.{Types, XTSeries}
import org.scalaml.workflow.data.{DataSource,DocumentsSource}
import scala.collection.mutable.ArrayBuffer
import Types.ScalaMl._
import org.scalaml.filtering.SimpleMovingAverage
import SimpleMovingAverage._
import scala.collection.immutable.HashSet
import org.scalaml.supervised.bayes.NaiveBayes
import org.scalaml.util.Display
import org.apache.log4j.Logger
import org.scalaml.stats.Stats
import org.scalaml.supervised.bayes.text.TermsScore




object TextBayesEval {
	final val pathName = "resources/text/chap5/"
		
	private val logger = Logger.getLogger("TextBayesEval")
	private def toDate(date: String): Long = {
		val idx1 = date.indexOf(".")
		val idx2 = date.lastIndexOf(".")
		if( idx1 != -1 && idx2 != -1) (date.substring(0, idx1) + date.substring(idx1+1, idx2)).toLong else -1
	}

    final val LEXICON = Map[String, String](
	   "tesla" -> "Tesla", "tsla" -> "TSLA" , "musk" -> "Musk", "china" -> "China", "chinese" -> "China", 
	   "charging" -> "Charge",  "supercharger" -> "Charge", "battery" -> "battery", "batteries" ->"battery", 
	   "upside" ->"Upside", "outperform" ->"Upside", "risk" -> "risk", "risks" ->"risk", "risky"->"risk", 
	   "panasonic" ->"Panasonic", "growth" -> "Upside", "short" -> "Downside", "shorted" -> "Downside", 
	   "downside" -> "Downside", "underperform" -> "Downside"
	)
	
	def toWords(content: String): Array[String] = {
    	val regExpr = "['|,|.|?|!|:|\"]"
    	content.trim.toLowerCase.replace(regExpr," ").split(" ").filter( _.length > 2)
    }
    
    	/**
    	 * Qoutes for TLSA stocks
    	 */
    final val TSLA_QUOTES = Array[Double](
    	250.56, 254.84, 252.66, 252.94, 246.21, 238.84, 234.41, 241.49, 237.79, 230.97, 233.98, 240.04, 235.84,
        234.91, 228.89, 220.17, 220.44, 212.96, 207.32, 212.37, 208.45, 216.97, 230.29, 225.4, 212.22, 207.52,
        215.46, 216.93, 204.19, 203.78, 198.09, 193.91, 199.11, 198.12, 204.38, 218.64, 207.99, 207.86, 199.85 )
    
        
	def run: Unit  = {
    	Display.show("Evaluation Multinomial Naive Bayes for text analysis", logger)
    	
		DocumentsSource(pathName) |> match {
		   case Some(corpus) => {
			  TermsScore[Long](toDate, toWords, LEXICON).score(corpus) match {
		  	  	case Some(terms) => {
		  	  		
		  	  	  var prevQ = 0.0
		  	  	  val diff = TSLA_QUOTES.map(q => {
		  	  	  	  val delta = if(q > prevQ) 1 else 0
		  	  	  	  prevQ = q
		  	  	  	  delta
		  	  	  })
		  		  	  	  	  	  
		  	  	  val relFrequencies = terms.toOrderedArray
		  	  	                      .zip(diff)
		  	  	                      .map(x => (x._1._2, x._2))
		  	  	                      .map(lbl => { 
		  	  	                      	  (LEXICON.values.toArray.map(f => if( lbl._1.contains(f) ) lbl._1(f) else 0.0), lbl._2) 
		  	  	                      })
			  	  val xt = XTSeries[(Array[Double], Int)](relFrequencies)
		  	  	  val nb = NaiveBayes[Double](xt)
			  	  Display.show(nb.toString, logger)
		  	  	}
		  	  	case None => Display.error("Naive Bayes failed", logger); None
		  	  }
		   }
		   case None => Display.error("Failed to load text", logger); None
		}
	}
}



object App1 extends App {
	   TextBayesEval.run
}


// -----------------------------  EOF ------------------------------------
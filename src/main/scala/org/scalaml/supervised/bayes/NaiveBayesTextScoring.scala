/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.supervised.bayes

import org.scalaml.workflow.data.TextSource
import NaiveBayesTextScoring._
import org.scalaml.util.Counter

import scala.collection.mutable.HashMap
import scala.annotation.implicitNotFound



import TextSource._
@implicitNotFound("Ordering not explicitly defined for NewsTerm class")
class NewsTerms[T <% Long](implicit val order: Ordering[T]) {
	import scala.collection.mutable.ArrayBuffer
	
	private[this] val newsArticles = new HashMap[T, Map[String, Double]]
	
	def += (date: T, weightedTerms: Map[String, Double]): Unit = {
	   require( date != null, "Incorrect date format")
	   require(weightedTerms != null && weightedTerms.size > 0, "Cannot update daily keywords with undefined document weighted words")
	   
	  def merge(m1: Map[String, Double], m2: Map[String, Double]): Map[String, Double] = {
  	     (m1.keys ++ m2.keys).foldLeft(new HashMap[String, Double])( (m, x) => {
	  		 var wt = 0.0
	  		 if(m1.contains(x)) wt += m1.get(x).get
	  		 if(m2.contains(x)) wt += m2.get(x).get
	  		 m.put(x, wt) 
  		     m 
  	     }).toMap
      }
	   
	  newsArticles.put(date, if( newsArticles.contains(date)) merge(newsArticles.get(date).get, weightedTerms) else weightedTerms )
	}
	
	def toOrderedArray: Array[(T, Map[String, Double])] = newsArticles.toArray.sortWith( _._1 < _._1)

	
	
	def display(labels: Iterable[String], el: (T, Map[String, Double])): String = {
	   val buf = new StringBuilder
	   val idx = el._1.toString
	   buf.append(idx.charAt(0)+"/" + idx.charAt(1) + idx.charAt(2))
	           .append(",") 
                 .append(labels.foldLeft(new StringBuilder)((buf, lbl) => buf.append( if( el._2.contains(lbl) ) el._2.get(lbl).get.toString else "0.0").append(",") ).toString)
	   buf.toString
    }
	
	def display(labels: Iterable[String]): String = {
	   import scala.collection.mutable.HashSet

       val mapped = labels.foldLeft(new HashSet[String])((set, lbl) => {set.add(lbl); set})
	   val buf = new StringBuilder("id")
	   buf.append(mapped.foldLeft(new StringBuilder)((buf, lbl) => buf.append(",").append(lbl)).toString).append("\n")
	   buf.append(toOrderedArray.foldLeft(new StringBuilder)((buf, dk) => buf.append(display(mapped, dk)).append("\n")).toString)
	   buf.toString
	}
}

object NewsTerms {
  type TimeStamp = Int
  def apply[T <% Long](implicit order: Ordering[T]): NewsTerms[T] = new NewsTerms[T]
}



@implicitNotFound("Ordering not explicitly defined for NaiveBayesTextScoring class")
class NaiveBayesTextScoring[T <% Long](val tStamp: String =>T, 
		          val words: String => Array[String], 
		          val lexicon: Map[String, String])(implicit val order: Ordering[T]) {
  require(tStamp != null, "Cannot score a text without a time stamp extractor")
  require(words != null, "Cannot score a text without a word extractor")
  require(lexicon != null, "Cannot score a text without a lexicon")
	
   
   	/**
   	 * Main scoring method for a corpus defined as as time stamp,
   	 * a title and a content
   	 */
   import NewsTerms._
   def score(corpus: Corpus): Option[NewsTerms[T]] = {
  	   rank(corpus) match {
  	  	 case Some(docs) => {
  	  	    try {
  	  	    	val cnts = docs.map( doc => (doc._1, count(doc._3)) )
  	  	    	val totalCnts = cnts.map( _._2).foldLeft(new Counter[String])((s, cnt) => s ++ cnt) 
  	  	    	val newTerms = NewsTerms[T]
  	  	    	cnts.foreach(cnt => newTerms += (cnt._1, (cnt._2/totalCnts).toMap))
  	  	    	Some(newTerms)
  	  	    }
  	  	    catch {
  	  	    	case e: IllegalArgumentException => println(e.toString); None
  	  	    	case e: IllegalStateException => println(e.toString); None
  	  	    }
  	  	 }
  	  	 case None => None
  	   }
   }

   private[this] def count(text: String): Counter[String] = {
  	 require(text != null && text.length > 0, "Cannot count the number of words in undefined text")
   
  	  words(text).foldLeft(new Counter[String])((cnt, w) => if( lexicon.contains(w)) cnt + lexicon.get(w).get else cnt )
   }
   
   private[this] def rank(corpus: Corpus): Option[CorpusType[T]] = {
  	  require( corpus!= null && corpus.size > 0, "Cannot order an undefined corpus of document")
   	  try {
	     Some(corpus.map(doc => {
		  	 val rank = tStamp(doc._1.trim)
		  	 (rank, doc._2, doc._3)
	     }).sortWith( _._1 < _._1))
   	  }
      catch {
      	 case e: NumberFormatException => None
      }
   }
}
   
   
object NaiveBayesTextScoring {
    type CorpusType[T] = Array[(T, String, String)]
    def apply[T <% Long](tStamp: String =>T, words: String => Array[String], dict: Map[String, String])(implicit ordering: Ordering[T]): NaiveBayesTextScoring[T] = new NaiveBayesTextScoring[T](tStamp, words, dict)
}


// ----------------------------  EOF ------------------------------------------
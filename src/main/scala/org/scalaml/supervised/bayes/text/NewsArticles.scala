/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.supervised.bayes.text

import org.scalaml.workflow.data.DocumentsSource
import TermsScore._
import org.scalaml.util.Counter
import scala.collection.mutable.HashMap
import scala.annotation.implicitNotFound
import scala.util.{Try, Success, Failure}
import DocumentsSource._
import org.scalaml.util.Display
import org.apache.log4j.Logger


@implicitNotFound("NewsArticle. Ordering not explicitly defined")
class NewsArticles[T <% Long](implicit val order: Ordering[T]) {
	import scala.collection.mutable.ArrayBuffer

	private[this] val articles = new HashMap[T, Map[String, Double]]
	
	def += (date: T, weightedTerms: Map[String, Double]): Unit = {
	   require( date != null, "Incorrect date format")
	   require(weightedTerms != null && weightedTerms.size > 0, "Cannot update daily keywords with undefined document weighted words")
	   
	  def merge(m1: Map[String, Double], m2: Map[String, Double]): Map[String, Double] = {
  	     (m1.keySet ++ m2.keySet).foldLeft(new HashMap[String, Double])( (m, x) => {
	  		 var wt = 0.0
	  		 if(m1.contains(x)) wt += m1(x)
	  		 if(m2.contains(x)) wt += m2(x)
	  		 m.put(x, wt) 
  		     m 
  	     }).toMap
      }
	   
	  articles.put(date, if( articles.contains(date)) merge(articles(date), weightedTerms) 
	  		             else weightedTerms )
	}
	
	def toOrderedArray: Array[(T, Map[String, Double])] = articles.toArray.sortWith( _._1 < _._1)

	
	
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


object NewsArticles {
  def apply[T <% Long](implicit order: Ordering[T]): NewsArticles[T] = new NewsArticles[T]
}



// ----------------------------  EOF ------------------------------------------
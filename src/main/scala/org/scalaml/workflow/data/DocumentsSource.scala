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
package org.scalaml.workflow.data

import scala.util.Try
import org.apache.log4j.Logger
import org.scalaml.core.Types
import org.scalaml.util.DisplayUtils
import DocumentsSource._
import org.scalaml.core.ETransform
import java.text.SimpleDateFormat


case class Document[T <% Long](date: T, title: String, content: String) {
  override def toString: String = s"date: ${date.toString}, title: $title, content: $content"
}

		/**
		 * Class that define the extraction of a document from a corpus
		 * or a list of text file using the following format:
		 * date
		 * Title
		 * Content
		 * @constructor Create a source for a set of documents in a given relative path.
		 * @throws IllegalArgumentException if the path is not defined
		 * @param pathName Relative path for the directory containing the corpus.
		 * @author Patrick Nicolas
		 * @since January 15, 2014
		 * @note Scala for Machine Learning Chapter 5 Naive Bayes models
		 */
final class DocumentsSource(
		dateFormat: SimpleDateFormat,
		val pathName: String) extends ETransform[SimpleDateFormat](dateFormat) {
	require( !pathName.isEmpty, 
			"DocumentsSource Cannot create a data source with undefined path")
	   
	private val logger = Logger.getLogger("TextSource")
	
	type U = Option[Long]
	type V = Corpus[Long]
	
	private[this] val filesList: Option[Array[String]] = {   
		val file = new java.io.File(pathName)
		if( file.isDirectory) 
			Some( file.listFiles.map( _.getName) ) 
		else 
			None
	}
    
		/**
		 * Extract a Corpus from a set documents located in pathName directory.
		 * @return corpus of document grouped and ordered by date.
		 */
	override def |> : PartialFunction[U, Try[V]] = {
	  case t: U if (filesList != None) => Try( if(t == None ) getAll else get(t) )
	}
	
	
	private def get(t: U): V = getAll.filter( _.date == t.get)
	
	
	private def getAll: V = {
		import scala.io.Source
		import java.io.{FileNotFoundException, IOException}
  	  
		filesList.get.map( fName => {
			val src = Source.fromFile(s"${pathName}${fName}")	
			val fieldIter = src.getLines
	  	  	  
			val date = nextField(fieldIter)
			val title = nextField(fieldIter)
			val content = fieldIter.map( _.trim).mkString 
			src.close
					
			if( date == None || title == None) 
			  throw new IllegalStateException("DocumentsSource: date undefined")
			val _date: Long = dateFormat.parse(date.get).getTime
			val doc = Document[Long](_date, title.get, content.toString)
			doc
		})
	}
   

	private def nextField(iter: Iterator[String]): Option[String] = iter.find( !_.isEmpty)
}


	/**
	 * Companion object for the Document Source
	 */
object DocumentsSource {
  type Corpus[T] = Seq[Document[T]]
	
		/**
		 * A corpus is defined as a sequence of {stringized data, title, content} tuples
		 */
	def apply(dateFormat: SimpleDateFormat, pathName: String): DocumentsSource = 
	  	new DocumentsSource(dateFormat, pathName)
}


// ---------------------------------  EOF --------------------------------------------------
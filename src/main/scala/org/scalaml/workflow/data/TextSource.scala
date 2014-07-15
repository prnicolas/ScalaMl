/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.workflow.data


import TextSource._
class TextSource(val pathName: String) {
   require(pathName != null && pathName.length > 1, "Cannot create a data source with undefined path")
	   
   val filesList: Array[String] = {   
  	  val file = new java.io.File(pathName)
  	  if( file.isDirectory) file.listFiles.map( x => x.getName) else Array[String](pathName)
   }
    
   def load: Option[Corpus] = {
  	  import scala.io.Source
  	  import java.io.{FileNotFoundException, IOException}
  	  
  	  try {
  	    Some(filesList.map( fName => {
	  	      val src = Source.fromFile(pathName + fName)	
	  	  	  val fieldIter = src.getLines
	  	  	  
	  	  	  val date = iterate(fieldIter)
	  	  	  val title = iterate(fieldIter)
	  	  	  val content = fieldIter.foldLeft(new StringBuilder)((b, str) => b.append(str.trim))
	  	  	 
	  	  	  src.close
	  	  	  (date, title, content.toString)
	  	  }))
  	  }
  	  catch {
  	  	case e: IllegalStateException => None
  	  	case e: FileNotFoundException => None
  	  	case e: IOException => None
  	  }
   }
   
   @inline
   private def iterate(iter: Iterator[String]): String = {
  	 var line: String = null
  	 while( iter.hasNext && (line == null || line.length == 0) )
  		line = iter.next.trim
     line
  }
}



object TextSource {
	type Corpus = Array[(String, String, String)]
	def apply(pathName: String): TextSource = new TextSource(pathName)
}

// ---------------------------------  EOF --------------------------------------------------
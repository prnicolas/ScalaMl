/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap12.spark


import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import scala.io.Source

import java.io.{FileNotFoundException, IOException}
import org.scalaml.workflow.data.DataSource



class DataExtractor(private val pathName: String, val suffix: String, private val reverseOrder: Boolean, val topLines: Int = 0)(implicit sc: SparkContext)  {
   require ( pathName != null & pathName.length > 1, "Cannot load data from undefined path")
    
    protected val filesList: Array[String] = {   
  	  val file = new java.io.File(pathName)
  	  if( file.isDirectory) {
  	      if( suffix != null) 
  	         file.listFiles.map( _.getName).filter(  _.endsWith(suffix)) 
  	       else file.listFiles.map( _.getName)
  	  }
  	  else 
  	     Array[String](pathName)
   }
   
   
   private[this] def load(fileName: String): Option[Array[(String, String)]] = {
     import java.io.{IOException, FileNotFoundException}
     
     try {
        val src = Source.fromFile(fileName)
        val results = if( topLines > 0)
           if( reverseOrder ) src.getLines.toArray.drop(topLines).reverse else src.getLines.toArray.drop(topLines) 
        else 
           if( reverseOrder ) src.getLines.toArray.reverse else src.getLines.toArray	  	
	  	src.close
	    Some(results.map( line => {val arr = line.split(","); (arr(0), arr(1)) }) )
     }
     catch {
        case e: IOException => { Console.println(e.toString); None }
        case e: FileNotFoundException => { Console.println(e.toString); None }
        case e: Exception => { Console.println(e.toString); None }
     }
   }
   
   def load(implicit f: String => Double): Option[RDD[(Long, Double)]] = {
      require(f != null, "Cannot extracts fields with undefined extractors")
     
      load(filesList(0)) match {
        case Some(fileContent) => { 
          val typedContent = fileContent map { c => (c._1.toLong, c._2.toDouble)}
          val rdd = sc.parallelize(typedContent)
          rdd.cache
          Some(rdd)  
        }
        case None => None
      }
   }
}


object DataExtractor {
   def apply(pathName: String, suffix: String, reverseOrder: Boolean)(implicit sc: SparkContext): DataExtractor  = new DataExtractor(pathName, suffix, reverseOrder)
}

// ----------------------------------  EOF ---------------------------------------------------

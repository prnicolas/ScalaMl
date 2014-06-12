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



	/**
	 * <P>Data extractor used to load and consolidate multiple data source (CSV files).</p>
	 * @param pathName relative path for data sources
	 * @param suffix suffix for the data files
	 * @param reversedOrder specify that the order of the data in the CSV file has to be revered before processing
	 * @param header number of lines dedicated to header information (usually 0 if pure data file, 1 for column header name)
	 * @exception IllegalArgumentException if the pathName or the file suffix is undefined.
	 * 
	 * @author Patrick Nicolas
	 * @date April 1, 2014
	 * @project Scala for Machine Learning
	 */
class DataExtractor(private val pathName: String, val suffix: String, private val reverseOrder: Boolean, val header: Int = 0)(implicit sc: SparkContext)  {
   require ( pathName != null & pathName.length > 1, "Cannot load data from undefined path")
   require ( suffix != null & suffix.length > 1, "Cannot load data from undefined file suffix")
       
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
        val results = if( header > 0)
           if( reverseOrder ) src.getLines.toArray.drop(header).reverse else src.getLines.toArray.drop(header) 
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
   
   		/**
   		 * <p>Load input data from a source of CSV files defined in the constructors and
   		 * apply a conversion function for selected field.</p>
   		 * @param f implicit conversion from String to Double
   		 * @return a Spark RDD as a list of index, value pair
   		 */
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

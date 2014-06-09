/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.workflow.data


import org.scalaml.core.XTSeries
import java.io.{FileNotFoundException, IOException}
import org.scalaml.stats.Stats
import scala.Array.canBuildFrom
import org.scalaml.workflow.PipeOperator
import scala.io.Source

		/**
		 * Generic class to load or save files into either HDFS or local files system
		 */
class DataSource(val pathName: String, 
				 val normalize: Boolean, 
				 val reverseOrder: Boolean) extends PipeOperator[List[Array[String] =>Double], List[XTSeries[Double]]] {
   import scala.io.Source
   
   final val CSV_DELIM = ","
   type TxtFields = Array[String]
   
   val fileList: Array[String] = {
  	  import java.io.File
   
  	  val file = new File(pathName)
  	  if( file.isDirectory)
         file.listFiles.map( x => x.getName)
      else
         Array[String](pathName)
   }
  		  /**
		   * Load data from a comma delimited fields text file and generate
		   * a tuple of column names and array of text fields
		   */
   private def load: Option[(TxtFields, Array[TxtFields])] = {
     import java.io.{IOException, FileNotFoundException}
     require ( pathName != null & pathName.length > 1, "Cannot load data from undefined path")
     
     try {
    	 val src = Source.fromFile(pathName)
	  	 val fields = src.getLines.map( _.split(CSV_DELIM)).toArray.drop(1)
	  	 val results = if( reverseOrder ) fields.reverse else  fields
	  	 val textFields = (fields(0), results)
	  	 src.close
	     Some(textFields)
     }
     catch {
        case e: IOException => { Console.println(e.toString); None }
        case e: FileNotFoundException => { Console.println(e.toString); None }
        case e: Exception => { Console.println(e.toString); None }
     }
   }
   
   
   import scala.collection.mutable.ArraySeq
   def loadConvert[T](implicit c: String => T): Option[List[ArraySeq[T]]] = {
  	  try {
    	 val src = Source.fromFile(pathName)
	  	 val fields: List[ArraySeq[T]] = src.getLines.map( _.split(CSV_DELIM).map(c(_))).toList
	     Some(fields)
     }
     catch {
        case e: IOException => { Console.println(e.toString); None }
        case e: FileNotFoundException => { Console.println(e.toString); None }
        case e: Exception => { Console.println(e.toString); None }
     }
   }
   			/**
   			 * Load, extracts, convert and normalize a list of fields using an extractors.
   			 * @return list of array of values or an empty list if either the input file is not found or the extraction and
   			 * conversion of some of the text fields failed.
   			 */
   override def |> (extr: List[TxtFields =>Double]): Option[List[XTSeries[Double]]] = {
     require(extr != null && extr.size > 0, "Cannot extracts fields with undefined extractors")
       	 
  	 load match {
       case Some(data) => {
          if( normalize) {
          	 import org.scalaml.stats.Stats
             Some(extr map {t => XTSeries[Double]((Stats[Double])( data._2.map(t(_) ) ).normalize) })
          }
          else
             Some(extr map {t => XTSeries[Double](data._2.map(t(_) ) ) })
       }
       case None => None
     }
   }
   

   def |> (extr: TxtFields => Double): Option[XTSeries[Double]] = {
     require(extr != null, "Cannot extracts fields with undefined extractors")
  	 
  	 load match {
       case Some(data) => {
          if( normalize) {
          	  import org.scalaml.stats.Stats
	          Some(XTSeries[Double](Stats[Double](  data._2.map( extr( _)) ).normalize))
          }
          else 
             Some(XTSeries[Double](data._2.map( extr(_))))
       }
       case None => None
     }
   }
}



object DataSource {
     
   def listSymbols(directoryName: String): Array[String] = {
        val directory = new java.io.File(directoryName)
        directory.listFiles.map( _.getName)
    }
  
  def apply(pathName: String, normalize: Boolean, reverseOrder:Boolean): DataSource = new DataSource(pathName, normalize, reverseOrder)
  def apply(pathName: String, normalize: Boolean): DataSource = new DataSource(pathName, normalize, true)
  def apply(fileName: String, pathName: String, normalize: Boolean): DataSource = new DataSource(fileName, normalize, true)
}

// ----------------------------------   EOF ----------------------------------------------
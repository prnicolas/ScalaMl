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
import org.scalaml.core.Types.ScalaMl._

		/**
		 * <p>Generic class to load or save files into either HDFS or local files system.</p>
		 * @param pathName relative path for the data files
		 * @param normalize flag to normalize data within the range [0,1]
		 * @param reverseOrder flag to re-order/index the data from the last entry to the first entry
		 * @param headerLines number of header lines in the file
		 * @exception IllegalArgumentException if the path name is undefined
		 * 
		 * @author Patrick Nicolas
		 * @date December 11, 2013
		 * @project Scala for Machine Learnin
		 */
class DataSource(val pathName: String, 
				 val normalize: Boolean, 
				 val reverseOrder: Boolean,
				 val headerLines: Int = 1) extends PipeOperator[List[Array[String] =>Double], List[XTSeries[Double]]] {
	
   require(pathName != null && pathName.length > 1, "Cannot create a data source with undefined path")
   require(headerLines >=0,  "Cannot create a data source with negative number of lines for header " + headerLines)
   
   import scala.io.Source
   
   final val CSV_DELIM = ","
   type TxtFields = Array[String]
   
   val fileList: Array[String] = {
  	  import java.io.File
   
  	  val file = new File(pathName)
  	  if( file.isDirectory)
         file.listFiles.map( _.getName)
      else
         Array[String](pathName)
   }
  		  /**
		   * Load data from a comma delimited fields text file and generate
		   * a tuple of column names and array of text fields
		   */
   private def load: Option[(TxtFields, Array[TxtFields])] = {
     import java.io.{IOException, FileNotFoundException}
     
     try {
    	 val src = Source.fromFile(pathName)
	  	 val fields = src.getLines.map( _.split(CSV_DELIM)).toArray.drop(headerLines)
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
   			 * @param ext function to extract and convert a list of comma delimited fields into a double
   			 * @return list of array of values or an empty list if either the input file is not found or the extraction and
   			 * conversion of some of the text fields failed.
   			 * @exception IllegalArgumentException if the extraction function is undefined
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

   
  def load(extr: TxtFields => DblVector): Option[XTSeries[DblVector]] = {
     require(extr != null, "Cannot extracts fields with undefined extractors")
  	 
  	 load match {
       case Some(data) => {
          if( normalize) 
          	  XTSeries.normalize(XTSeries[DblVector](data._2.map( extr(_))))
          else 
             Some(XTSeries[DblVector](data._2.map( extr(_))))
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
  
  def apply(pathName: String, normalize: Boolean, reverseOrder:Boolean, headerLines: Int): DataSource = new DataSource(pathName, normalize, reverseOrder, headerLines)
  def apply(pathName: String, normalize: Boolean): DataSource = new DataSource(pathName, normalize, true)
  def apply(fileName: String, pathName: String, normalize: Boolean): DataSource = new DataSource(fileName, normalize, true)
}

// ----------------------------------   EOF ----------------------------------------------
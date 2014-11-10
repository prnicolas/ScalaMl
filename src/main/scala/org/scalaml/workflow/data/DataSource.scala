/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95c
 */
package org.scalaml.workflow.data


import org.scalaml.core.XTSeries
import java.io.{FileNotFoundException, IOException}
import org.scalaml.stats.Stats
import scala.Array.canBuildFrom
import org.scalaml.core.design.PipeOperator
import scala.io.Source
import org.scalaml.core.types.ScalaMl._
import org.apache.log4j.Logger
import scala.util.{Try, Failure, Success}
import org.scalaml.util.Display


		/**
		 * <p>Generic class to load or save files into either HDFS or local files system.</p>
		 * @param pathName relative path for the data files
		 * @param normalize flag to normalize data within the range [0,1]
		 * @param reverseOrder flag to re-order/index the data from the last entry to the first entry
		 * @param headerLines number of header lines in the file
		 * @throws IllegalArgumentException if the path name is undefined
		 * 
		 * @author Patrick Nicolas
		 * @since December 11, 2013
		 * @note Scala for Machine Learning
		 */
final class DataSource(val pathName: String, 
				       val normalize: Boolean, 
				       val reverseOrder: Boolean,
				       val headerLines: Int = 1,
				       val srcFilter: Option[Array[String] => Boolean]= None) extends PipeOperator[List[Array[String] =>Double], List[DblVector]] {
	
   require(pathName != null && pathName.length > 1, "Cannot create a data source with undefined path")
   require(headerLines >=0,  s"Cannot create a data source with negative number of lines for header $headerLines")
   
   import scala.io.Source
   
   private val logger = Logger.getLogger("DataSource")
   final val CSV_DELIM = ","
   type TxtFields = Array[String]
   
   val fileList = {
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
     Try {
    	 val src = Source.fromFile(pathName)
	  	 val rawFields = src.getLines.map( _.split(CSV_DELIM)).toArray.drop(headerLines)
	  	 
	  	 val fields = if( srcFilter != None) rawFields.filter(srcFilter.get) else rawFields
	  	 val results = if( reverseOrder ) fields.reverse else  fields
	  	 val textFields = (fields(0), results)
	  	 src.close
	     textFields
     } match {
    	 case Success(textFields) => Some(textFields)
    	 case Failure(e) => Display.error("DataSource.load ", logger, e); None
     }
   }
   
   
   import scala.collection.mutable.ArraySeq
   def loadConvert[T](implicit c: String => T): Option[List[ArraySeq[T]]] = {
  	  Try {
    	 val src = Source.fromFile(pathName)
	  	 val fields = src.getLines.map( _.split(CSV_DELIM).map(c(_))).toList
	     src.close
	     fields
     } match {
    	 case Success(fields) => Some(fields)
    	 case Failure(e) => Display.error("DataSource.loadConvert ", logger, e); None
     }
   }
   
   			/**
   			 * Load, extracts, convert and normalize a list of fields using an extractors.
   			 * @param ext function to extract and convert a list of comma delimited fields into a double
   			 * @return list of array of values or an empty list if either the input file is not found or the extraction and
   			 * conversion of some of the text fields failed.
   			 * @throws IllegalArgumentException if the extraction function is undefined
   			 */
   override def |> : PartialFunction[List[TxtFields => Double], List[DblVector]] = {
     case extr: List[TxtFields => Double] if(extr != null && extr.size > 0) => {
       load match {
         case Some(data) => {
             if( normalize) {
          	 import org.scalaml.stats.Stats
                extr map {t => Stats[Double](data._2.map(t(_))).normalize }
             }
             else
               extr map {t => data._2.map(t(_) ) }
         }
         case None => Display.error("DataSource.|> ", logger); List.empty
       }
     }
   }


   def |> (extr: TxtFields => Double): XTSeries[Double] = {
     require(extr != null, "Cannot extracts fields with undefined extractors")
  	 
  	 load match {
       case Some(data) => {
          if( normalize) {
          	 import org.scalaml.stats.Stats
	         XTSeries[Double](Stats[Double](  data._2.map( extr( _)) ).normalize)
          }
          else 
             XTSeries[Double](data._2.map( extr(_)))
       }
       case None => Display.error("DataSource.|> ", logger); XTSeries.empty
     }
   }
   
   def extract : Option[DblVector] = 
  	  Try {
    	 Source.fromFile(pathName)
	           .getLines
	           .drop(headerLines)
	           .map( _.toDouble ).toArray
     } match {
    	 case Success(x) => Some(x)
    	 case Failure(e) => Display.none("DataSource.load ", logger, e)
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
       case None => Display.none("DataSource.load ", logger)
     }
   }
}



object DataSource {
     
	/**
	 * <p>Generate a list of CSV files within a directory, associated with a list of symbol:<br>
	 * symbol => directoryName/symbol.csv
	 * @param directoryName, name of the directory containing the CSV files
	 */
   def listSymbolFiles(directoryName: String): Array[String] = {
       require(directoryName != null, "DataSource.listSymbolFiles Directory name is undefined")
       
       val directory = new java.io.File(directoryName)
       val filesList =  directory.listFiles
       if( filesList != null)  directory.listFiles.map( _.getName) else Array.empty
    }
  
  def apply(pathName: String, normalize: Boolean, reverseOrder:Boolean, headerLines: Int, filter: Option[Array[String] => Boolean]): DataSource = new DataSource(pathName, normalize, reverseOrder, headerLines, filter)  
  def apply(pathName: String, normalize: Boolean, reverseOrder:Boolean, headerLines: Int): DataSource = new DataSource(pathName, normalize, reverseOrder, headerLines, None)
  def apply(pathName: String, normalize: Boolean): DataSource = new DataSource(pathName, normalize, true)
  def apply(symName: String, pathName: String, normalize: Boolean): DataSource = new DataSource(s"$pathName$symName", normalize, true)
}

// ----------------------------------   EOF ----------------------------------------------
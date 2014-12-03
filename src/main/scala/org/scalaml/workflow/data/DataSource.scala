/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96d
 */
package org.scalaml.workflow.data


import org.scalaml.core.XTSeries
import java.io.{FileNotFoundException, IOException}
import org.scalaml.stats.Stats
import scala.Array.canBuildFrom
import org.scalaml.core.design.PipeOperator
import scala.io.Source
import org.scalaml.core.Types.ScalaMl._
import org.apache.log4j.Logger
import scala.util.{Try, Failure, Success}
import org.scalaml.util.Display
import DataSource._

		/**
		 * <p>Generic class to load or save files into either HDFS or local files system.</p>
		 * @constructor Create a data source. 
		 * @throws IllegalArgumentException if the path name is undefined or the headerLines value is out of range
		 * @param pathName Relative path for the data files.
		 * @param normalize Flag to normalize data within the range [0,1].
		 * @param reverseOrder Flag to re-order/index the data from the last entry to the first entry.
		 * @param headerLines Number of header lines in the file.
		 * @param srcFilter Source filter applied to the data source stream.
		 * @author Patrick Nicolas
		 * @since December 11, 2013
		 * @note Scala for Machine Learning
		 */
final class DataSource(	pathName: String, 
						normalize: Boolean, 
						reverseOrder: Boolean, 
						headerLines: Int = 1, 
						srcFilter: Option[Fields => Boolean]= None) 
						extends PipeOperator[List[Fields =>Double], List[DblVector]] {
	   
	import scala.io.Source
	check(pathName, headerLines)
	
	private val logger = Logger.getLogger("DataSource")
   
	val fileList = {
		import java.io.File
   
		val file = new File(pathName)
		if( file.isDirectory)
			file.listFiles.map( _.getName)
		else
			Array[String](pathName)
	}
   
		/**
		 * <p>Load and convert the content of a file into a list of fields. The fields
		 * are defined as a sequence of type T.</p>
		 * @param c implicit conversion of a String to a type.
		 * @return List of sequence of fields of the extraction has been successful, None otherwise
		 */
	import scala.collection.mutable.ArraySeq
	def loadConvert[T](implicit c: String => T): Option[List[ArraySeq[T]]] = {
		Try {
			val src = Source.fromFile(pathName)
			val fields = src.getLines.map( _.split(CSV_DELIM).map(c(_))).toList
			src.close
			fields
		} match {
			case Success(fields) => Some(fields)
			case Failure(e) => Display.none("DataSource.loadConvert ", logger, e)
		}
	}
   

		/**
		 * <p>Load, extracts, convert and normalize a list of fields using an extractors.</p>
		 *  @throws MatchError if the list of extractor Fields => Double is undefined or is empty
		 *  @return PartialFunction of list of extractor of type List[Fields => Double] as input and a corresponding list of floating point value as output
		 */
	override def |> : PartialFunction[List[Fields => Double], List[DblVector]] = {
		case extr: List[Fields => Double] if(extr != null && extr.size > 0) => {
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


		/**
		 * <p>Generate a Time series of single variables by applying a extractor that
		 * converts a line in a file to an array of String and to a Double.</p>
		 * @param extr Extractor that convert an array of fields(String) to a double
		 * @return A Single variable time series
		 * @throws IllegalArgumentException if the extractor is not defined.
		 */
	def |> (extr: Fields => Double): XTSeries[Double] = {
		require(extr != null, "DataSource.|> Cannot extracts fields with undefined extractors")
  	 
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
   
	
		/**
		 * <p>Extract a vector from a file of relative path, pathName.</p>
		 * @return A vector of double floating point values if successful, None otherwise
		 */
	def extract : Option[DblVector] = 
		Try (Source.fromFile(pathName).getLines.drop(headerLines).map( _.toDouble ).toArray ) match {
			case Success(x) => Some(x)
			case Failure(e) => Display.none("DataSource.load ", logger, e)
		}
  	 
   
  
	def load(extr: Fields => DblVector): XTSeries[DblVector] = {
		require(extr != null, "Cannot extracts fields with undefined extractors")
  	 
		load match {
			case Some(data) => {
				if( normalize) 
					XTSeries.normalize(XTSeries[DblVector](data._2.map( extr(_)))).get
				else 
					XTSeries[DblVector](data._2.map( extr(_)))
			}
			case None => Display.error("DataSource.load ", logger); XTSeries.empty
		}
	}
	
	
			/**
		 * Load data from a comma delimited fields text file and generate
		 * a tuple of column names and array of text fields
		 */
   private def load: Option[(Fields, Array[Fields])] = {     
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
			case Failure(e) => Display.none("DataSource.load ", logger, e)
		}
	}
}



object DataSource {
	final val CSV_DELIM = ","
	type Fields = Array[String]
  	
	private def check(pathName: String, headerLines: Int): Unit =  {
		require(pathName != null && pathName.length > 1, "DataSource.check Cannot create a data source with undefined path")
		require(headerLines >=0, s"DataSource.check  Cannot create a data source with negative number of lines for header $headerLines")
	}
     
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
  
	def apply(pathName: String, normalize: Boolean, reverseOrder:Boolean, headerLines: Int, filter: Option[Array[String] => Boolean]): DataSource = 
			new DataSource(pathName, normalize, reverseOrder, headerLines, filter)
	
	def apply(pathName: String, normalize: Boolean, reverseOrder:Boolean, headerLines: Int): DataSource = 
	  		new DataSource(pathName, normalize, reverseOrder, headerLines, None)
	
	def apply(pathName: String, normalize: Boolean): DataSource = 
	  		new DataSource(pathName, normalize, true)
	
	def apply(symName: String, pathName: String, normalize: Boolean): DataSource = 
	  		new DataSource(s"$pathName$symName", normalize, true)
}

// ----------------------------------   EOF ----------------------------------------------
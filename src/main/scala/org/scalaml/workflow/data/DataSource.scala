/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.workflow.data

import org.apache.log4j.Logger

import org.scalaml.core.{XTSeries, Types}
import org.scalaml.stats.Stats
import org.scalaml.core.Design.PipeOperator
import org.scalaml.core.Types.ScalaMl
import org.scalaml.util.DisplayUtils

import DataSource._, ScalaMl._

		/**
		 * <p>Generic class to load or save files into either HDFS or local files system. The
		 * data source loads content from 
		 * <ul>
		 *   <li>A file if the path name is a csv delimited file</li>
		 *   <li>A list of files if the path name is a directory contains csv delimited files</li>
		 *   <li>A list of csv delimited files associated to a list of symbol as 'symbol" => "symbol.csv"</li>
		 * </ul></p>
		 * @constructor Create a data source. 
		 * @throws IllegalArgumentException if the path name is undefined or the headerLines value 
		 * is out of range
		 * @param pathName Relative path for the data files.
		 * @param normalize Flag to normalize data within the range [0,1].
		 * @param reverseOrder Flag to re-order/index the data from the last entry to the first entry.
		 * @param headerLines Number of header lines in the file.
		 * @param srcFilter Source filter applied to the data source stream.
		 * 
		 * @author Patrick Nicolas
		 * @since December 11, 2013
		 * @note Scala for Machine Learning
		 */
final class DataSource(
		pathName: String, 
		normalize: Boolean, 
		reverseOrder: Boolean, 
		headerLines: Int = 1, 
		srcFilter: Option[Fields => Boolean]= None) 
				extends PipeOperator[List[Fields =>Double], List[DblVector]] {
	   
	import scala.io.Source
	import scala.util.{Try, Failure, Success}
	
	check(pathName, headerLines)
	
	private val logger = Logger.getLogger("DataSource")

		/**
		 * List of CSV files contained in a directory defined in the path if 
		 * it is a directory. The list contains the name of the path is 
		 * it is a file
		 */
	lazy val filesList: Option[Array[String]] = {
		import java.io.File
   
		Try {
			val file = new File(pathName)
			if( file.isDirectory)
				file.listFiles.map( _.getName)
			else
				Array[String](pathName)
		}
		match {
		  case Success( fileNames ) => Some(fileNames )
		  case Failure(e) => DisplayUtils.none("DataSource.fileList", logger)
		}
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
		} 
		match {
			case Success(fields) => Some(fields)
			case Failure(e) => DisplayUtils.none("DataSource.loadConvert ", logger, e)
		}
	}
   

		/**
		 * <p>Load, extracts, convert and normalize a list of fields using an extractors.</p>
		 *  @throws MatchError if the list of extractor Fields => Double is undefined or is empty
		 *  @return PartialFunction of list of extractor of type List[Fields => Double] as input and 
		 *  a corresponding list of floating point value as output
		 */
	override def |> : PartialFunction[List[Fields => Double], List[DblVector]] = {
		case extr: List[Fields => Double] if( !extr.isEmpty ) => {
			load.map( data => {
				if( normalize) {
						import org.scalaml.stats.Stats
						extr map {t => Stats[Double](data._2.map(t(_))).normalize }
				}
				else
					extr map {t => data._2.map(t(_) ) }
			}).getOrElse(List.empty)
		}
	}

		/**
		 * <p>Generate a Time series of single variables by applying a extractor that
		 * converts a line in a file to an array of String and to a Double.</p>
		 * @param extr Extractor that convert an array of fields(String) to a double
		 * @return A Single variable time series
		 * @throws IllegalArgumentException if the extractor is not defined.
		 */
	def |> (extr: Fields => Double): XTSeries[Double] = load.map( data =>  { 
		if( normalize) {
			import org.scalaml.stats.Stats
			XTSeries[Double](Stats[Double](  data._2.map( extr( _)) ).normalize)
		}
		else 
			XTSeries[Double](data._2.map( extr(_)))
	}).getOrElse(XTSeries.empty[Double])
   
	
		/**
		 * <p>Extract a vector from a file of relative path, pathName.</p>
		 * @return A vector of double floating point values if successful, None otherwise
		 */
	def extract: Option[DblVector] = 
		Try (Source.fromFile(pathName)
							.getLines
							.drop(headerLines)
							.map( _.toDouble ).toArray) 
		match {
			case Success(x) => Some(x)
			case Failure(e) => DisplayUtils.none("DataSource.load ", logger, e)
		}
  	 
   
  
	def load(extr: Fields => DblVector): XTSeries[DblVector] = {
		load.map( data => { 
		  if( normalize) 
					XTSeries.normalize(XTSeries[DblVector](data._2.map( extr(_)))).get
				else 
					XTSeries[DblVector](data._2.map( extr(_)))
		}).getOrElse(XTSeries.empty[DblVector])
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
		} 
		match {
			case Success(textFields) => Some(textFields)
			case Failure(e) => DisplayUtils.none("DataSource.load ", logger, e)
		}
	}
}


		/**
		 * Companion object for the DataSource class. The singleton is used
		 * to define the DataSource constructors and validate their parameters
		 * 
		 * @author Patrick Nicolas
		 * @since December 11, 2013
		 * @note Scala for Machine Learning
		 */
object DataSource {
	final val CSV_DELIM = ","
	type Fields = Array[String]

		/**
		 * <p>Generate a list of CSV files within a directory, associated with a list of symbol:<br>
		 * symbol => directoryName/symbol.csv
		 * @param directoryName, name of the directory containing the CSV files
		 */
	def listSymbolFiles(directoryName: String): Array[String] = {
		require(directoryName != Types.nullString, "DataSource.listSymbolFiles Directory name is undefined")

		val directory = new java.io.File(directoryName)
		val filesList =  directory.listFiles
		if( !filesList.isEmpty )  directory.listFiles.map( _.getName) else Array.empty
	}

		/**
		 * Default constructor for the DataSource
		 * @param pathName Relative path for the data files.
		 * @param normalize Flag to normalize data within the range [0,1].
		 * @param reverseOrder Flag to re-order/index the data from the last entry to the first entry.
		 * @param headerLines Number of header lines in the file.
		 * @param srcFilter Source filter applied to the data source stream.
		 */
	def apply(
			pathName: String, 
			normalize: Boolean, 
			reverseOrder:Boolean, 
			headerLines: Int, 
			filter: Option[Array[String] => Boolean]): DataSource = 
					new DataSource(pathName, normalize, reverseOrder, headerLines, filter)

		/**
		 * Constructor for the DataSource without field filtering
		 * @param pathName Relative path for the data files.
		 * @param normalize Flag to normalize data within the range [0,1].
		 * @param reverseOrder Flag to re-order/index the data from the last entry to the first entry.
		 * @param headerLines Number of header lines in the file.
		 */
	def apply(
			pathName: String, 
			normalize: Boolean, 
			reverseOrder:Boolean, 
			headerLines: Int): DataSource = 
		new DataSource(pathName, normalize, reverseOrder, headerLines, None)
	
		/**
		 * Constructor for the DataSource without field filtering, headerlines. The extraction
		 * of the content does not alter the order of the datarows in the file.
		 * @param pathName Relative path for the data files.
		 * @param normalize Flag to normalize data within the range [0,1].
		 */
	def apply(pathName: String, normalize: Boolean): DataSource = 
		new DataSource(pathName, normalize, true, 1, None)

		/**
		 * Constructor for the DataSource without field filtering, headerlines. The extraction
		 * of the content does not alter the order of the datarows in the file.
		 * @param symName Name of the symbol associated to a file in a directory for which the 
		 * content is to be extracted
		 * @param pathName Relative path for the data files.
		 * @param normalize Flag to normalize data within the range [0,1].
		 */
	def apply(symName: String, pathName: String, normalize: Boolean): DataSource = 
		new DataSource(s"$pathName$symName", normalize, true, 1, None)

	private def check(pathName: String, headerLines: Int): Unit =  {
		require(pathName != Types.nullString, 
				"DataSource.check Undefined path for data source")
		require(headerLines >=0, 
		   	s"DataSource.check Incorrect number of header lines $headerLines for data source")
	}
}

// ----------------------------------   EOF ----------------------------------------------
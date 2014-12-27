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

import scala.util.{Try, Success, Failure}

import org.apache.log4j.Logger
import org.scalaml.core.XTSeries
import org.scalaml.core.Types
import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.Design.PipeOperator
import org.scalaml.util.{DisplayUtils, FileUtils}
import ScalaMl._


		/**
		 * <p>Generic class to load or save files into either HDFS or local files system. The persistency
		 * of data is defined as a data transformation and therefore inherit from the PipeOperator</p>
		 * @constructor Create a DataSink transform associated to a specific path name or database name. 		 
		 * @throws IllegalArgumentException if the name of the storage is undefined
		 * @param sinkName Name of the storage (file, database, ..).
		 * @author Patrick Nicolas
		 * @since December 15, 2013
		 * @note Scala for Machine Learning
		 */
final protected class DataSink[T <% String](
		sinkName: String) extends PipeOperator[List[XTSeries[T]], Int] {
	import XTSeries._, DataSource._
	import scala.io.Source
	import java.io.File
	require(sinkName != Types.nullString, "DataSink Name of the storage is undefined")
	
	private val logger = Logger.getLogger("DataSink")	
	
		// Create an handle to a file the results needs to be dump.
		// The directory containing results is created if it does not exist
	private val sinkPath: Option[File] = 
		Try {
			val path = sinkName.substring(0, sinkName.lastIndexOf("/")-1)
			val dir = new File(path)
			if( !dir.exists )
				dir.mkdir
			dir
		} match {
		  case Success(f) => Some(f)
		  case Failure(e) => DisplayUtils.none("DataSink.sinkFile", logger, e)
		}
   
	
    
	
		/**
		 * <p>Write the content into the storage with sinkName as identifier.</p>
		 * @param content Stringized data to be stored
		 * @return true if the content has been successfully stored, false otherwise
		 * @throws IllegalArgumentException If the content is not defined.
		 */
	def write(content: String): Boolean = {
		require(content.length > 0, "DataSink.write Content undefined")
		assert(sinkPath != None, "DataSink.write Sink path undefined")
		
		FileUtils.write(content, sinkName, "DataSink")
	}

		/**
		 * <p>Write the content of a vector into the storage with sinkName as identifier.</p>
		 * @param vector of type Double to be stored
		 * @return true if the vector has been successfully stored, false otherwise
		 * @throws IllegalArgumentException If the vector is either undefined or empty.
		 */
	def write(v: DblVector) : Boolean = {
		require( !v.isEmpty, "DataSink.write Cannot persist an undefined vector")
		assert( sinkPath != None, "DataSink.write undefined sink path")
		
		val content = v.mkString(CSV_DELIM)
		this.write(content.substring(0, content.length-1))
	}
	
		/**
		 * <p>Persists a set of time series into a predefined storage, sinkFile. The 
		 * results are written one line per time series </p>
		 * @throws MatchError if the list of time series is either undefined or empty
		 * @return PartialFunction of a list of parameterized time series as input and the number 
		 * of time series saved as output
		 */
	override def |> : PartialFunction[List[XTSeries[T]], Int] = {
		case xs: List[XTSeries[T]] if( !xs.isEmpty && sinkPath != None) => {
			import java.io.PrintWriter
			
			var printWriter: Option[PrintWriter] = None
			Try {
				val content = new StringBuilder
				val numValues = xs(0).size-1
				val last = xs.size-1
					// Write into file with one time series per line
				var k = 0
				while( k < numValues) {
					val values = xs.toArray
					Range(0, last) foreach(j => content.append(s"${values(j)(k)},") )
					content.append(s"${values(last)(k)}\n")
					k += 1
				}
				
				printWriter = Some(new PrintWriter(sinkName))
				printWriter.map( _.write(content.toString))
				k
			} 
			match {
				case Success(k) => k
				case Failure(e) => {
					DisplayUtils.error("DataSink.|> ", logger)
	    		  
					if( printWriter != None) {
						Try {printWriter.map(_.close); 1 }
						match {
							case Success(res) => res
							case Failure(e) => DisplayUtils.error("DataSink.|> ", logger)
						}
					}
					else DisplayUtils.error("DataSink.|> no printWrite", logger)
				}
			}
		}
	}
}


		/**
		 * <p>Companion object to the class DataSink used to defined its constructor.</p>
		 */
object DataSink {
	import scala.annotation.implicitNotFound
  
		/**
		 * <p>Create a DataSink with an implicit conversion of the type parameter to a string.</p>
		 * @param sinkPath name of the storage.
		 */
	@implicitNotFound("DataSink.apply Conversion of paramerized type undefined")
	def apply[T](sinkPath: String)(implicit f: T => String= (t:T) => t.toString): DataSink[T] 
		= new DataSink[T](sinkPath)
}

// ----------------------------------   EOF ----------------------------------------------
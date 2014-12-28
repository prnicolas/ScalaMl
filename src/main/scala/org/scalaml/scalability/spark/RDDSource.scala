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
package org.scalaml.scalability.spark

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import scala.annotation.implicitNotFound
import scala.io.Source
import java.io.{FileNotFoundException, IOException}
import org.scalaml.workflow.data.DataSource
import org.scalaml.core.Design.PipeOperator
import org.scalaml.core.Types
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.XTSeries
import org.scalaml.util.DisplayUtils
import org.apache.log4j.Logger
import org.apache.spark.rdd.EmptyRDD


		/**
		 * <p>Class to encapsulate a simple configuration of an RDD.</p>
		 * @param cache Flag to specify if the RDD should be cached (maintained) in memory
		 * @param persist define the storage level for the RDD
		 * 
		 * @author Patrick Nicolas
		 * @since April 1, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks / Apache Spark
		 */
case class RDDConfig(cache: Boolean, persist: StorageLevel)


		/**
		 * <P>Data extractor used to load and consolidate multiple data source (CSV files).</p>
		 * @constructor Create a RDD associated to a source of data of type DataSource
		 * @param pathName      Relative path for data sources
		 * @param normalize    Flag to specify normalize of data [0, 1]
		 * @param reversedOrder Specify that the order of the data in the CSV file has to be 
		 * reversed before processing
		 * @param headerLines  Number of lines dedicated to header information (usually 0 if pure 
		 * data file, 1 for column header name)
		 * @param config  Configuration for the Spark RDDs
		 * @throws IllegalArgumentException if the pathName, the file suffix is undefined or the 
		 * number of header lines is negative
		 * @see org.scalaml.workflow.data.DataSource
		 * @see org.apache.spark.rdd._
		 * 
		 * @author Patrick Nicolas
		 * @since April 1, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks / Apache Spark
		 */
@implicitNotFound("Spark context is implicitly undefined")
final class RDDSource(
		pathName: String, 
		normalize: Boolean, 
		reverseOrder: Boolean, 
		headerLines: Int, 
		config: RDDConfig)
		(implicit sc: SparkContext)	extends PipeOperator[Array[String] =>DblVector, RDD[DblVector]] {

	import RDDSource._
	check(pathName, headerLines)
	
	private val src = DataSource(pathName, normalize, reverseOrder, headerLines)
	private val logger = Logger.getLogger("RDDSource")

		/**
		 * Load, extracts, convert and normalize a list of fields using an extractors.
		 * @param ext function to extract and convert a list of comma delimited fields into a 
		 * vector of Doubles
		 * @return a RDD of DblVector if succeed, None otherwise
		 * @throws IllegalArgumentException if the extraction function is undefined
		 */   
	override def |> : PartialFunction[(Array[String] => DblVector), RDD[DblVector]] = {
		case extractor: (Array[String] => DblVector) if( src.filesList != None) => {
			
			val ts = src load extractor
			assert(!ts.isEmpty, s"RDDSource.|> Could not extract time series from $pathName" )
			
		  val rdd = sc.parallelize(ts.toArray)
			rdd.persist(config.persist)
				
			if( config.cache)
				rdd.cache
			rdd
		}
	}
}


   
		/**
		 * Companion object to RDDSource used to defines several version of constructors
		 * and the conversion from time series to RDD
		 * @author Patrick Nicolas
		 * @since April 1, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks / Apache Spark
		 */
object RDDSource {
	import org.apache.spark.mllib.linalg.{Vector, DenseVector}

		/**
		 * <p>Default RDD configuration as persistent and caching in memory only.</p>
		 */
	final val DefaultRDDConfig = new RDDConfig(true, StorageLevel.MEMORY_ONLY)
	
	def apply(
			pathName: String, 
			normalize: Boolean, 
			reverseOrder: Boolean, 
			headerLines: Int, 
			state: RDDConfig)
			(implicit sc: SparkContext): RDDSource  = 
					new RDDSource(pathName, normalize, reverseOrder, headerLines, state: RDDConfig)
	
	
	def apply(
			pathName: String, 
			normalize: Boolean, 
			reverseOrder: Boolean, 
			headerLines: Int)
			(implicit sc: SparkContext): RDDSource  = 
					new RDDSource(pathName, normalize, reverseOrder, headerLines, DefaultRDDConfig)
   
		/**
		 * <p>Converts a time series of vectors of double to a Spark RDD using a predefined
		 * set of RDD configuration parameters (Caching, Persistence).</p>
		 * @param xt time series to be converted into a RDD
		 * @param rddConfig configuration parameters used in the conversion
		 * @throws IllegalArgumentException if the time series or the RDD configuration 
		 * argument is not defined
		 * @throws ImplicitNotFoundException if the Spark context has not been defined.
		 */
	@implicitNotFound("Spark context is implicitly undefined")
	def convert(
			xt: XTSeries[DblVector], 
			rddConfig: RDDConfig) (implicit sc: SparkContext): RDD[Vector] = {
	  
		require( !xt.isEmpty, 
				"RDDSource.convert Cannot generate a RDD from undefined time series")

		val rdd: RDD[Vector] = sc.parallelize(xt.toArray.map(new DenseVector(_)))
		rdd.persist(rddConfig.persist)
		if( rddConfig.cache)
			rdd.cache
		rdd
	}
   
   
	private def check(pathName: String, headerLines: Int): Unit = {
		require(pathName == Types.nullString, 
				"RDDSource.check Cannot create a RDD source with undefined path name")
		require(headerLines >= 0, 
				s"Cannot generate a RDD from an input file with $headerLines header lines")
	}
}

// ----------------------------------  EOF ---------------------------------------------------

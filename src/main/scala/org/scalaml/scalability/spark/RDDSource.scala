/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99
 */
package org.scalaml.scalability.spark

	// Scala standard library
import scala.annotation.implicitNotFound
import scala.collection._
import scala.io.Source
import scala.util.Try
import java.io.{FileNotFoundException, IOException}

	// 3rd party library
import org.apache.spark.SparkContext
import org.apache.spark.rdd.{EmptyRDD, RDD}
import org.apache.spark.storage.StorageLevel
import org.apache.log4j.Logger


	// Scala for machine classes
import org.scalaml.workflow.data.DataSource
import org.scalaml.core.{Types, ETransform}
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.stats.XTSeries
import org.scalaml.util.DisplayUtils



		/**
		 * Class to encapsulate a simple configuration of an RDD.
		 * @param cache Flag to specify if the RDD should be cached (maintained) in memory
		 * @param persist define the storage level for the RDD
		 * @author Patrick Nicolas
		 * @since April 1, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks / Apache Spark
		 */
case class RDDConfig(cache: Boolean, persist: StorageLevel)


		/**
		 * <P>Data extractor used to load and consolidate multiple data source (CSV files).
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
		 * @see Scala for Machine Learning Chapter 12 Scalable frameworks / Apache Spark
		 */
@throws(classOf[IllegalArgumentException])
@implicitNotFound(msg = "Spark context is implicitly undefined")
final class RDDSource(
		pathName: String, 
		normalize: Boolean, 
		reverseOrder: Boolean, 
		headerLines: Int, 
		config: RDDConfig)
		(implicit sc: SparkContext)	extends ETransform[RDDConfig](config) {

	import RDDSource._
	
	type U = Array[String] => DblArray
	type V = RDD[DblArray]
	check(pathName, headerLines)

	private val src = DataSource(pathName, normalize, reverseOrder, headerLines)
	private val logger = Logger.getLogger("RDDSource")

		/**
		 * Load, extracts, convert and normalize a list of fields using an extractors.
		 * @param ext function to extract and convert a list of comma delimited fields into a 
		 * vector of Doubles
		 * @return a RDD of DblArray if succeed, None otherwise
		 * @throws IllegalArgumentException if the extraction function is undefined
		 */   
	override def |> : PartialFunction[U, Try[V]] = {
		case extractor: U if( src.filesList != None) => {
	 
			src.load(extractor).map( ts => {
				val rdd: RDD[DblArray] = sc.parallelize(ts.toSeq)
				rdd.persist(config.persist)
				
				if( config.cache)
					rdd.cache
				rdd
			})
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
		 * Default RDD configuration as persistent and caching in memory only.
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
		 * Converts a time series of vectors of double to a Spark RDD using a predefined
		 * set of RDD configuration parameters (Caching, Persistence).
		 * @param xt time series to be converted into a RDD
		 * @param rddConfig configuration parameters used in the conversion
		 * @throws IllegalArgumentException if the time series or the RDD configuration 
		 * argument is not defined
		 * @throws ImplicitNotFoundException if the Spark context has not been defined.
		 */
	@implicitNotFound(msg = "Spark context is implicitly undefined")
	def convert(
			xt: immutable.Vector[DblArray], 
			rddConfig: RDDConfig) (implicit sc: SparkContext): RDD[Vector] = {
	  
		require( !xt.isEmpty, 
				"RDDSource.convert Cannot generate a RDD from undefined time series")

		val rdd: RDD[Vector] = sc.parallelize(xt.toVector.map(new DenseVector(_)))
		rdd.persist(rddConfig.persist)
		if( rddConfig.cache)
			rdd.cache
		rdd
	}
   
   
	private def check(pathName: String, headerLines: Int): Unit = {
		require( !pathName.isEmpty, 
				"RDDSource.check Cannot create a RDD source with undefined path name")
		require(headerLines >= 0, 
				s"Cannot generate a RDD from an input file with $headerLines header lines")
	}
}

// ----------------------------------  EOF ---------------------------------------------------

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
package org.scalaml.scalability.spark


import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import scala.annotation.implicitNotFound
import scala.io.Source
import java.io.{FileNotFoundException, IOException}
import org.scalaml.workflow.data.DataSource
import org.scalaml.core.design.PipeOperator
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.XTSeries
import org.scalaml.util.Display
import org.apache.log4j.Logger
import org.apache.spark.rdd.EmptyRDD


case class RDDConfig(cache: Boolean, persist: StorageLevel)


	/**
	 * <P>Data extractor used to load and consolidate multiple data source (CSV files).</p>
	 * @constructor Create a RDD associated to a source of data of type DataSource
	 * @param pathName      Relative path for data sources
	 * @param normalize    Flag to specify normalize of data [0, 1]
	 * @param reversedOrder Specify that the order of the data in the CSV file has to be revered before processing
	 * @param headerLines  Number of lines dedicated to header information (usually 0 if pure data file, 1 for column header name)
	 * @param config  Configuration for the Spark RDDs
	 * 
	 * @throws IllegalArgumentException if the pathName or the file suffix is undefined.
	 * @see org.scalaml.workflow.data.DataSource
	 * 
	 * @author Patrick Nicolas
	 * @since April 1, 2014
	 * @note Scala for Machine Learning Chapter 12 Scalable frameworks/Apache Spark
	 */
@implicitNotFound("Spark context is implicitly undefined")
final class RDDSource(pathName: String, normalize: Boolean, reverseOrder: Boolean, headerLines: Int, config: RDDConfig)(implicit sc: SparkContext)  
				extends PipeOperator[Array[String] =>DblVector, RDD[DblVector]] {

	import RDDSource._
	check(pathName, headerLines, config)
	
	private val src = DataSource(pathName, normalize, reverseOrder, headerLines)
	private val logger = Logger.getLogger("RDDSource")

		/**
		 * Load, extracts, convert and normalize a list of fields using an extractors.
		 * @param ext function to extract and convert a list of comma delimited fields into a double vector
		 * @return a RDD of DblVector if succeed, None otherwise
		 * @throws IllegalArgumentException if the extraction function is undefined
		 */   
	override def |> : PartialFunction[(Array[String] => DblVector), RDD[DblVector]] = {
		case extractor: (Array[String] => DblVector) if(extractor != null) => {
			val ts = src load extractor
			if( ts != XTSeries.empty ) {
				val rdd = sc.parallelize(ts.toArray)
				rdd.persist(config.persist)
				if( config.cache)
					rdd.cache
				rdd
			}
			else { Display.error("RDDSource.|> ", logger); sc.emptyRDD } 
		}
	}
}


   
	/**
	 * Companion object to RDDSource used to defines several version of constructors
	 * and the conversion from time series to RDD
	 */
object RDDSource {
	import org.apache.spark.mllib.linalg.{Vector, DenseVector}

		/**
		 * <p>Default RDD configuration as persistent and caching in memory only.</p>
		 */
	final val DefaultRDDConfig = new RDDConfig(true, StorageLevel.MEMORY_ONLY)
	
	def apply(pathName: String, normalize: Boolean, reverseOrder: Boolean, headerLines: Int, state: RDDConfig)(implicit sc: SparkContext): RDDSource  = new RDDSource(pathName, normalize, reverseOrder, headerLines, state: RDDConfig)
	def apply(pathName: String, normalize: Boolean, reverseOrder: Boolean, headerLines: Int)(implicit sc: SparkContext): RDDSource  = new RDDSource(pathName, normalize, reverseOrder, headerLines, DefaultRDDConfig)
   
		/**
		 * <p>Converts a time series of vectors of double to a Spark RDD using a predefined
		 * set of RDD configuration parameters (Caching, Persistency).</p>
		 * @param xt time series to be converted into a RDD
		 * @param rddConfig configuration parameters used in the conversion
		 * @throws IllegalArgumentException if the time series or the RDD configuration argument is not defined
		 * @throws ImplicitNotFoundException if the Spark context has not been defined.
		 */
	@implicitNotFound("Spark context is implicitly undefined")
	def convert(xt: XTSeries[DblVector], rddConfig: RDDConfig)(implicit sc: SparkContext): RDD[Vector] = {
		require(xt != null && xt.size > 0, "RDDSource.convert Cannot generate a RDD from undefined time series")
		require(rddConfig != null, "RDDSource.convert  Cannot generate a RDD from a time series without an RDD configuration")

		val rdd: RDD[Vector] = sc.parallelize(xt.toArray.map( x => new DenseVector(x)))
		rdd.persist(rddConfig.persist)
		if( rddConfig.cache)
			rdd.cache
		rdd
	}
   
   
	private def check(pathName: String, headerLines: Int, config: RDDConfig) {
		require(pathName != null && pathName.length > 2, "Cannot create a RDD source with undefined path name")
		require(headerLines >= 0, s"Cannot generate a RDD from an input file with $headerLines header lines")
		require(config != null, "Cannot create a RDD source for undefined stateuration")
	}
}

// ----------------------------------  EOF ---------------------------------------------------

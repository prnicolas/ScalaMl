/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.scalability.spark


import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel

import scala.annotation.implicitNotFound
import scala.io.Source
import java.io.{FileNotFoundException, IOException}

import org.scalaml.workflow.data.DataSource
import org.scalaml.workflow.PipeOperator
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.XTSeries


case class RDDConfig(val cache: Boolean, val persist: StorageLevel)


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
@implicitNotFound("Spark context is implicitly undefined")
class RDDSource(val pathName: String, 
		        val normalize: Boolean, 
		        val reverseOrder: Boolean, 
		        val headerLines: Int,
		        val config: RDDConfig)(implicit sc: SparkContext)  
		          extends PipeOperator[Array[String] =>DblVector, RDD[DblVector]] {
	             
   private val src = DataSource(pathName, normalize, reverseOrder, headerLines)
  
   
      	/**
   		 * Load, extracts, convert and normalize a list of fields using an extractors.
   		 * @param ext function to extract and convert a list of comma delimited fields into a double vector
   		 * @return a RDD of DblVector if succeed, None otherwise
   		 * @exception IllegalArgumentException if the extraction function is undefined
   		 */
   override def |> (extr: Array[String] =>DblVector): Option[RDD[DblVector]] = {
     require(extr != null, "Cannot generate list of RDDs with undefined extractors")
     
     src.load(extr) match {
       case Some(xt) => {
      	   val rdd = sc.parallelize( xt.toArray)
      	   rdd.persist(config.persist)
           if( config.cache)
          	   rdd.cache
           Some(rdd) 
       }
       case None => None
     }
   }
}


   
	/**
	 * Companion object to RDDSource used to defines several version of constructors
	 * and the conversion from time series to RDD
	 */
object RDDSource {
   final val DefaultRDDConfig = new RDDConfig(true, StorageLevel.MEMORY_ONLY)
   def apply(pathName: String, normalize: Boolean, reverseOrder: Boolean, headerLines: Int, config: RDDConfig)(implicit sc: SparkContext): RDDSource  = new RDDSource(pathName, normalize, reverseOrder, headerLines, config: RDDConfig)
   def apply(pathName: String, normalize: Boolean, reverseOrder: Boolean, headerLines: Int)(implicit sc: SparkContext): RDDSource  = new RDDSource(pathName, normalize, reverseOrder, headerLines, DefaultRDDConfig)
   
   @implicitNotFound("Spark context is implicitly undefined")
   def convert(xt: XTSeries[DblVector], rddConfig: RDDConfig)(implicit sc: SparkContext): RDD[DblVector] = {
  	 require(xt != null && xt.size > 0, "Cannot generate a RDD from undefined time series")
     require(rddConfig != null, "Cannot generate a RDD from a time series without an RDD configuration")
     
     val rdd = sc.parallelize( xt.toArray)
     rdd.persist(rddConfig.persist)
     if( rddConfig.cache)
         rdd.cache
     rdd
   }
   
}

// ----------------------------------  EOF ---------------------------------------------------

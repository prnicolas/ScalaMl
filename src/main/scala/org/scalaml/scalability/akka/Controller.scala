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
package org.scalaml.scalability.akka

import scala.collection.mutable.ListBuffer

import akka.actor.Actor

import org.scalaml.core.Design.PipeOperator
import org.scalaml.core.XTSeries.DblSeries
import org.scalaml.core.Types.ScalaMl.DblVector

		/**
		 * <p>Generic controller actor that defines the three key elements of a distributed 
		 * data transformation</p>
		 *  @constructor Create a controller for data transformations: 
		 *  @throws IllegalArgumentException if the time series is undefined or empty
		 *  @param xt Time series to be processed
		 *  @param fct Data transformation of type PipeOperator
		 *  @param partitioner Methodology to partition a time series in segments or partitions to be 
		 *  processed by workers.
		 *  
		 *  @author Patrick Nicolas
		 *  @since March 30, 2014
		 *  @note Scala for Machine Learning Chapter 12 Scalable Frameworks / Akka
		 */
abstract class Controller(
		protected val xt: DblSeries, 
		protected val fct: PipeOperator[DblSeries, DblSeries], 
		protected val partitioner: Partitioner
		) extends Actor {
	
	require( !xt.isEmpty, 
			"Master.check Cannot create the master actor, undefined time series")
}


		/**
		 * <p>Class that create partition or sub-time series from a initial
		 * time series. The number of partitions defines the number of concurrent
		 * tasks in the data processing of the time series.<br>
		 * <b>numPartitions</b> Number of partitions used in concurrent processing
		 * @constructor Create a partitioner for a give number of partitions.
		 * @throws IllegalArgumentException if the number of partition is out of range
		 * 
		 * @author Patrick Nicolas
		 * @since March 24, 2014
		 * @note Scala for Machine Learning Chapter 12 Scalable Framework/Akka/Master-workers
		 */
final class Partitioner(val numPartitions: Int) {
	require(numPartitions > 1 && numPartitions < 128, 
			s"Partitioner Number of partitions $numPartitions is out of range")
	
		/**
		 * Method to split a given time series into 'numPartitions' for concurrent processing
		 * @param xt Time series to split for concurrent processing
		 * @return Sequence of absolute index in the time series associated with each partition.
		 * @throws IllegalArgumentException if the time series argument is undefined.
		 */
	def split(xt: DblSeries): Array[Int] = {
		require( !xt.isEmpty, "Partitioner.split Cannot partition undefined time series")	
		
			// Compute the size of each partition
		val sz = (xt.size.toDouble/numPartitions).floor.toInt
		
			// Compute the absolute index in the original time series for each partition
		val indices = Array.tabulate(numPartitions)(i=>(i+1)*sz)
		indices.update(numPartitions -1, xt.size)
		indices
	}
}


// -----------------------------------------  EOF ------------------------------------------------
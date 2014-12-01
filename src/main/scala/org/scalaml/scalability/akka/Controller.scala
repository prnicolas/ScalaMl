/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96c
 */
package org.scalaml.scalability.akka

import org.scalaml.core.design.PipeOperator
import org.scalaml.core.XTSeries._
import akka.actor.Actor


		/**
		 * <p>Generic controller actor that defines the three key elements of a distributed data transformation</p>
		 *  @constructor Create a controller for data transformations: 
		 *  @throws IllegalArgumentException if one of the class parameters are undefined
		 *  @param xt Time series to be processed
		 *  @param fct Data transformation of type PipeOperator
		 *  @param partitioner Methodology to partition a time series in segments or partitions to be processed by workers.
		 *  
		 *  @author Patrick Nicolas
		 *  @since March 30, 2014
		 *  @note Scala for Machine Learning Chapter 12 Scalable Frameworks/Akka
		 */			
abstract class Controller(	protected val xt: DblSeries, 
							protected val fct: PipeOperator[DblSeries, DblSeries], 
							protected val partitioner: Partitioner) extends Actor {
	
	require(xt != null && xt.size > 0, "Master.check Cannot create a master actor to process undefined time series")
	require(fct != null, "Master.check Cannot create a master actor with undefined data transformation function")
	require(partitioner != null, "Master.check Cannot create a master actor with undefined data partitioner")
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
class Partitioner(val numPartitions: Int) {
	require(numPartitions > 1 && numPartitions < 128, s"Partitioner Number of partitions $numPartitions is out of range")
	
		/**
		 * Method to split a given time series into 'numPartitions' for concurrent processing
		 * @param xt Time series to split for concurrent processing
		 * @return Sequence of absolute index in the time series associated with each partition.
		 * @throws IllegalArgumentException if the time series argument is undefined.
		 */
	def split(xt: DblSeries): Array[Int] = {
		require(xt != null && xt.size > 0, "Partitioner.split Cannot partition undefined time series")
		
		val sz = (xt.size.toDouble/numPartitions).floor.toInt
		val indices = Array.tabulate(numPartitions)(i=>(i+1)*sz)
		indices.update(numPartitions -1, xt.size)
		indices
	}
}


// -----------------------------------------  EOF ------------------------------------------------
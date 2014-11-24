/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.scalability.akka


import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ArrayBuffer
import org.scalaml.core.types.ScalaMl._
import org.scalaml.scalability.akka.message._
import akka.actor._
import akka.util.Timeout
import org.scalaml.core.XTSeries
import org.scalaml.core.design.PipeOperator
import XTSeries._
import org.scalaml.util.Display
import org.scalaml.filtering.DFT
import scala.collection.mutable.ListBuffer
import org.apache.log4j.Logger



		/**
		 * <p>Generic distributed transformation of time series using futures and callbacks.<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
		 * <b>xt</b>          Time series to be processed.
		 * <b>fct</b>         Data transformation
		 * <b>partitioner</b> Method to partition time series for concurrent processing.
		 * </span></pre></p>
	     *  @constructor Create a distributed transformation for time series. 
		 *  @throws IllegalArgumentException if the class parameters are either undefined or out of range.
		 *  @author Patrick Nicolas
		 *  @since March 30, 2014
		 *  @note Scala for Machine Learning Chapter 12 Scalable Frameworks/Akka/Futures
		 */			
abstract class TransformFuturesClbck(_xt: DblSeries, _fct: PipeOperator[DblSeries, DblSeries], _partitioner: Partitioner) 
									extends Controller(_xt, _fct, _partitioner) {
	private val logger = Logger.getLogger("TransformFuturesClbck")
	
		/**
		 * <p>Message handling for the future-based controller for the transformation of time series.</p>
		 * <b>Start</b> to initiate the future computation of transformation of time series.<.p>
		 */
	override def receive = {
		case Start => compute(transform)
		case _ => Display.error("TransformFuturesClbck.recieve Message not recognized", logger)
	}
  

	private def transform: Array[Future[DblSeries]] = {   
		val partIdx = partitioner.split(xt)
		val partitions = partIdx.map(n => XTSeries[Double](xt.slice(n - partIdx(0), n).toArray))
		val futures = new Array[Future[DblSeries]](partIdx.size)

		partitions.zipWithIndex.foreach(pi => {
			futures(pi._2) = Future[DblSeries] { fct |> pi._1 }
		}) 
		futures
	}
	
		/**
		 * <p>Executes the aggregation of the results for all the future execution of 
		 * the transformation of time series, using the transform fct.</p>
		 * @param futures Set of future transformation of time series using the transform fct.
		 * @throws IllegalArgumentException if futures are undefined
		 */
	private def compute(futures: Array[Future[DblSeries]]): Seq[Double] = {
		require(futures != null && futures.size > 0, "Cannot delegate computation to undefined futures")
  	  
		val aggregation = new ArrayBuffer[DblSeries]
		futures.foreach(f => { 
			f onSuccess {
				case data: DblSeries => aggregation.append(data)
			}
			f onFailure {
				case e: Exception => {
					Display.error("TransformFuturesClbck.aggregate failed", logger, e)
					aggregation.append(XTSeries.empty)
				}
			}
		}) 

		if( aggregation.find( _.isEmpty) == None)
			aggregate(aggregation.toArray)
		else
			Seq.empty
	}
	
		/**
		 * <p>Executes the aggregation of the results for all the future execution of 
		 * the transformation of time series, using the transform fct.</p>
		 * @param results of the distributed processing of the time series by futures
		 * @throws IllegalArgumentException if the results are undefined
		 */
	protected def aggregate(results: Array[DblSeries]): Seq[Double] 
}



// ------------------------  EOF -----------------------------------------------------------------------------
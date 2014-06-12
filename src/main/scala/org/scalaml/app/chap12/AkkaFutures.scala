/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap12


import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ArrayBuffer
import org.scalaml.core.Types.ScalaMl._
import akka.actor._
import akka.util.Timeout
import org.scalaml.stats.Stats


case class Launch(val id: Int = 0)


		/**
		 * <p>Base class for the two versions of using futures to balance folds for cross
		 * validation. The folds are balanced by minimizing the variance of data on each
		 * fold. The two versions are<br>
		 *  blocking the caller<br>
		 *  Callback the caller.</p>
		 *  @param data time series {x, y} used in the cross validation and to be broken down into balance folds
		 *  @param numFutures number of futures used in the parallelization of the computation
		 *  @exception IllegalArgumentException if the class parameters are either undefined or out of range.
		 *  
		 *  @see org.scalaml.app.chap12.FoldsNormalizer
		 *  @author Patrick Nicolas
		 *  @data March 30, 2014
		 *  @project Scala for Machine Learning
		 */
abstract class GroupsNormalizerActor(val data: XYTSeries,
                      			 val numFutures: Int) extends Actor {
	
	require(data != null && data.size > 0, "Cannot balance undefined data across folds")
	require(numFutures > 0 && numFutures <32, "Number of futures in balancing folds " +  numFutures + " is out of range")

		
	implicit val timeout = Timeout(2000)	
	val normalizer = new GroupsNormalizer(numFutures, data)


	override def preStart: Unit = { }
    override def postStop: Unit = {  }
	    
       /**
        * <pMain event loop used to distribute the computation of the
        * variance of each fold to each future. The only message process is
        * the launch of the computation of the variance.</p>
        */
	def receive = {
	   case launch: Launch => compute(normalizer.groups)
       case _ => println("Message not recognized")
	}

	
	protected def execute(futures: Array[Future[Double]]): Double
	
	private[this] def compute(data: Array[Array[(XY, Int)]]): Unit = {   
	   val futures = new Array[Future[Double]](numFutures)
	      
	   Range(0, numFutures) foreach( i => {
		  futures(i) = Future[Double] { 
  	         val xSeries = data(i).map( _._1._1 )
  	         val ySeries = data(i).map( _._1._2 )
  	         Stats[Double](xSeries).variance + Stats[Double](ySeries).variance
		  }
	   }) 
	   
	   execute(futures)
	}
}




		/**
		 * <p>Version of the future to compute the variance of data assigned to each group for which the
		 *  the execution blocks until all the future complete the computation of 
		 *  the variance for each assigned fold..</p>
		 *  @param data time series {x, y} used in the cross validation and to be broken down into normalized groups
		 *  @param numFutures number of futures used in the parallelization of the computation
		 *  @exception IllegalArgumentException if the class parameters are either undefined or out of range.
		 *  
		 *  @author Patrick Nicolas
		 *  @data March 30, 2014
		 *  @project Scala for Machine Learning
		 */			
final class GroupsNormalizerBlocking(val _data: XYTSeries,
                      				  val _numFutures: Int) extends GroupsNormalizerActor(_data, _numFutures) {
  
		/**
		 * <p>Delegates the computation of the variance of each group
		 * to their respective future. The execution blocks using Await.</p>
		 * @param futures set of futures used to compute the variance of each grou[
		 * @exception IllegalArgumentException if futures are undefined
		 */
   override def execute(futures: Array[Future[Double]]): Double = {
  	  require(futures != null && futures.size > 0, "Cannot delegate computation to undefined futures")
  	  
      val variances = Range(0, numFutures).map( i =>
         Await.result(futures(i), timeout.duration).asInstanceOf[Double]  )
      Stats[Double](variances.toArray).variance
   }
}


		/**
		 * <p>Version of the future computation of variance for which the execution
		 * get notified of the completion of each future by call back..</p>
		 *  @param data time series {x, y} used in the cross validation and to be broken down into balance folds
		 *  @param numFutures number of futures used in the parallelization of the computation
		 *  @exception IllegalArgumentException if the class parameters are either undefined or out of range.
		 *  
		 *  @author Patrick Nicolas
		 *  @data March 30, 2014
		 *  @project Scala for Machine Learning
		 */	
final class GroupsNormalizerCallback(val _data: XYTSeries,
                      			 val _numFutures: Int) extends GroupsNormalizerActor(_data, _numFutures) {
  
	
	    /**
		 * <p>Delegates the computation of the variance of each group
		 * to their respective future. The execution get notified through callbacks.</p>
		 * @param futures set of futures used to compute the variance of each group
		 * @exception IllegalArgumentException if futures are undefined
		 */
   override def execute(futures: Array[Future[Double]]): Double = {
  	 require(futures != null && futures.size > 0, "Cannot delegate computation to undefined futures")
  	   	  
  	 val variances = new DblVector(numFutures)
  	 
  	 Range(0, numFutures).foreach( i => { 
  		futures(i) onSuccess {
  		   case variance: Double => variances(i) = variance
  		}
  		futures(i) onFailure {
	       case e: Exception => Console.println("Failed with future " + i)
	    }
  	 })
  	 
  	 variances.find( _ == -1.0 ) match {
  		 case Some(found) => -1.0
  		 case None => Stats[Double](variances).variance
  	 }
   }
}


// ------------------------  EOF -----------------------------------------------------------------------------
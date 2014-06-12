/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap12


import scala.collection.mutable.ArrayBuffer
import org.scalaml.core.Types.ScalaMl._
import java.io.{IOException, PrintWriter}
import akka.actor._
import scala.util.Random
import org.scalaml.stats.Stats



		/**
		 * <p>Class to generate well balanced folds for cross validation. Well-balanced folds are created
		 * from a generic data set, broken down into N folds or segments then permuting or shuffling data
		 * between these segments in such a way that the variance between the folds or segments is minimal/</p>
		 * @param workers List of reference to the worker or slave actors.
		 * @param data Data set to be processed by the worker actors
		 * @exception IllegalArgumenException if the number of iterations or the number of workers is out of range
		 * 
		 * @author Patrick Nicolas
		 * @date March 23, 2014
		 * @project Scala for Machine Learning
		 */
protected class FoldsNormalizer(val numFolds: Int, val data: XYTSeries) {  
   final val BANDWIDTH = 35
      
   private var workersCnt = numFolds
   private val variances = new DblVector(numFolds)
   private var lowestVariance = Double.MaxValue
   private var bestFolds: Array[Array[(XY, Int)]] = null
   
   		/**
   		 * Definition of the folds created by broken down the original data set
   		 * into folds or segments of similar size, each of which is processed
   		 * by a dedicated worker actor.
   		 */
   val folds = {
  	  val _folds = new Array[Array[(XY, Int)]](numFolds)
  	  val indexedData = data.zipWithIndex
  	  val segSize: Int = ((data.size).toDouble/numFolds).floor.toInt
  	    
  	  Range(0, numFolds-1).foreach( n => {
  	     val lowBound = segSize*n
  	     _folds(n) = indexedData.slice(lowBound, segSize+lowBound) 
  	  })
  	  _folds(numFolds-1) = indexedData.drop(segSize*(numFolds-1))
  	  _folds
   }

   		/**
   		 * <p>Retrieve the best combination of folds to perform cross validation.
   		 * The folds are balanced by minimizing the variance between data sets
   		 * @return tuple of array of folds and variance associated with the fold
   		 */
   def normalizedFolds: (Array[Array[(XY, Int)]], Double) = (bestFolds, lowestVariance)
   
   
		 /**
		  * <p>Update composition of each fold and keep track of the most
		  * balanced folds. Balancing the folds to minimize variance is accomplished
		  * by shuffling randomly data between folds.</p>
		  * @param id id of the iteration
		  * @param variance variance related to a specific fold
		  * @return true if variance for all the folds has been computed (processed by
		  * all the worker actors.
		  */
   def update(id: Int, variance: Double): Boolean = { 
  	  if( id > 0)
  	  	shuffle(id)
  	  	
  	  workersCnt -= 1
  	  val allWorkersCompleted = workersCnt == 0
  	  variances(workersCnt) = variance
  	  
  	  if( allWorkersCompleted ) {
  	     val lastVariance = evaluateVariance
  	     println("Iteration: " + (id +1) + " cross validator variance: " + lastVariance)
  	     if( lastVariance < lowestVariance) {
  	      	lowestVariance = lastVariance
  	        bestFolds = folds.clone	
  	     }
  	     workersCnt = numFolds
  	  }
  	  allWorkersCompleted
   }
   
   
   private def shuffle(index: Int): Unit = {
       val lowBand = index*BANDWIDTH % data.size
       val highBand = (lowBand + BANDWIDTH) % data.size
       
       Range(lowBand, highBand).foreach ( n => {
  	  	 val fromBucketIdx = (n+1)%folds.size
  	  	 val toBucketIdx = n%folds.size
  	  	 val fromDataIdx = n % folds(fromBucketIdx).size
  	  	 val toDataIdx = n % folds(toBucketIdx).size
  	  	 
  	  	 folds(fromBucketIdx)(fromDataIdx) = folds(toBucketIdx)(toDataIdx) 
  	 })
   }
   
   private def evaluateVariance: Double =  Stats[Double](variances).variance
}


/*
protected class FoldsBalancer(val workers: List[ActorRef], val data: XYTSeries) {
   require(workers.size > 0 && workers.size < 32, "Number of worker actors " + workers.size + " in execution state in out of range" )    
   
   final val BANDWIDTH = 35
      
   private var workersCnt = workers.size
   private val variances = new DblVector(workers.size)
   private var lowestVariance = Double.MaxValue
   private var bestFolds: Array[Array[(XY, Int)]] = null
   
   		/**
   		 * Definition of the folds created by broken down the original data set
   		 * into folds or segments of similar size, each of which is processed
   		 * by a dedicated worker actor.
   		 */
   val folds = {
  	  val _folds = new Array[Array[(XY, Int)]](workers.size)
  	  val indexedData = data.zipWithIndex
  	  val segSize: Int = ((data.size).toDouble/workers.size).floor.toInt
  	    
  	  Range(0, workers.size-1).foreach( n => {
  	     val lowBound = segSize*n
  	     _folds(n) = indexedData.slice(lowBound, segSize+lowBound) 
  	  })
  	  _folds(workers.size-1) = indexedData.drop(segSize*(workers.size-1))
  	  _folds
   }

   		/**
   		 * <p>Retrieve the best combination of folds to perform cross validation.
   		 * The folds are balanced by minimizing the variance between data sets
   		 * @return tuple of array of folds and variance associated with the fold
   		 */
   def balancedFolds: (Array[Array[(XY, Int)]], Double) = (bestFolds, lowestVariance)
   
   
		 /**
		  * <p>Update composition of each fold and keep track of the most
		  * balanced folds. Balancing the folds to minimize variance is accomplished
		  * by shuffling randomly data between folds.</p>
		  * @param id id of the iteration
		  * @param variance variance related to a specific fold
		  * @return true if variance for all the folds has been computed (processed by
		  * all the worker actors.
		  */
   def update(id: Int, variance: Double): Boolean = { 
  	  if( id > 0)
  	  	shuffle(id)
  	  	
  	  workersCnt -= 1
  	  val allWorkersCompleted = workersCnt == 0
  	  variances(workersCnt) = variance
  	  
  	  if( allWorkersCompleted ) {
  	     val lastVariance = evaluateVariance
  	     println("Iteration: " + (id +1) + " cross validator variance: " + lastVariance)
  	     if( lastVariance < lowestVariance) {
  	      	lowestVariance = lastVariance
  	        bestFolds = folds.clone	
  	     }
  	     workersCnt = workers.size
  	  }
  	  allWorkersCompleted
   }
   
   
   private def shuffle(index: Int): Unit = {
       val lowBand = index*BANDWIDTH % data.size
       val highBand = (lowBand + BANDWIDTH) % data.size
       
       Range(lowBand, highBand).foreach ( n => {
  	  	 val fromBucketIdx = (n+1)%folds.size
  	  	 val toBucketIdx = n%folds.size
  	  	 val fromDataIdx = n % folds(fromBucketIdx).size
  	  	 val toDataIdx = n % folds(toBucketIdx).size
  	  	 
  	  	 folds(fromBucketIdx)(fromDataIdx) = folds(toBucketIdx)(toDataIdx) 
  	 })
   }
   
   private def evaluateVariance: Double =  Stats[Double](variances).variance
}

*/



// ---------------------------------  EOF -------------------------
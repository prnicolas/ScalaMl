/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap12


import org.scalaml.core.XTSeries
import scala.util.Random
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import com.typesafe.config.Config
import akka.actor.Props
import scala.concurrent.{Await, duration}
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.scalability.spark._
import org.apache.spark.SparkContext
import org.apache.spark.storage.StorageLevel
import org.scalaml.scalability.akka._
import org.scalaml.scalability.scala._



object SparkKMeansEval extends AkkaEval {
  final val K = 8
  final val numRuns = 16

  def run : Unit = {
  		  
  	 extractVolatilityVolume match {
		case Some(x) => {
		   val volatilityVol = XTSeries[DblVector](x(0).zip(x(1)).map( x => Array[Double](x._1, x._2)))
		   
		   val kmeansConfig = SparkKMeansConfig(K, numIters, numRuns)
		   implicit val sc = new SparkContext("Local", "SparkKMeans")  // no need to load additional jar file
		   
		   val rddConfig = RDDConfig(true, StorageLevel.MEMORY_ONLY)
		   val sparkKMeans = SparkKMeans(kmeansConfig, rddConfig, volatilityVol)
		   
		   val obs = Array[Double](0.23, 0.67)
		   sparkKMeans |> obs match {
		  	 case Some(clusterId) => println("(" + obs(0) + "," + obs(1) + ") = " + clusterId)
		  	 case None => println("Failed to predict data")
		   }
		   
		   val obs2 = Array[Double](0.56, 0.11)
		   sparkKMeans |> obs2 match {
		  	 case Some(clusterId) => println("(" + obs2(0) + "," + obs2(1) + ") = " + clusterId)
		  	 case None => println("Failed to predict data")
		   }
		}
		case None => println("Failed to extract data")
  	 }
  }
}

// ---------------------------------  EOF -------------------------
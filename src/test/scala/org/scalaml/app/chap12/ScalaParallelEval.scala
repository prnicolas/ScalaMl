/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
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




object ScalaParallelCollectionEval {
  final val descriptor = "Evaluation performance Scala parallel collections"	
	
  def run: Unit = {
     import scala.collection.parallel.mutable.ParArray
     import scala.collection.mutable.ArraySeq
     import scala.collection.parallel.ForkJoinTaskSupport
     import scala.concurrent.forkjoin.ForkJoinPool
     
     println(descriptor)

	 val sz = 100000
	 val data = Array.tabulate(sz) ( _ << 1)
		
	 data.par.map( _ >> 1)
	 data.par.reduceLeft( _ + _)
	
	 (100000 to 2000000 by 100000) foreach( i => {
	    val data = Array.tabulate( i) ( _ * 0.1)
	    val test = new ParallelCollections[Double](data, 4)
			
		test.map(x => 2.5*Math.sin(0.01*x) + 2.0*Math.cos(0.01*x))
		test.reduceLeft((s, x) => s + x)
	 })
  }
}


// ---------------------------------  EOF -------------------------
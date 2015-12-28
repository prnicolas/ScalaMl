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
package org.scalaml.app.chap4

import scala.util.{Random, Try}
import org.apache.log4j.Logger
import org.scalaml.stats.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.unsupervised.clustering.{Cluster, KMeans, KMeansConfig}
import org.scalaml.unsupervised.Distance._
import org.scalaml.util.LoggingUtils
import org.scalaml.app.Eval
import org.scalaml.util.Assertable
import LoggingUtils._, ScalaMl._, KMeans._



		/**
		 * '''Purpose:''' Singleton to validate and test the KMeans algorithm using
		 * a simple test case
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see org.scalaml.unsupervised.kmeans
		 * @see Scala for Machine Learning Chapter 4 ''Unsupervised learning'' Clustering / K-means
		 */
object KMeansEval2 extends Eval with Assertable {

		/**
		 * Name of the evaluation 
		 */
	val name: String = "KMeansEval2"
	protected val assertMsg: String = "K-means validation"
	final val SCALE = 100
	final val K = 3
	
		/**
		 * Random value generator 
		 */
	def fGen(id: Int): Double = SCALE*(id*10.0 + Random.nextDouble)
	
		/*
		 * Features vector
		 * @param id Identifier for the feature
		 * @param a first feature
		 * @param b second feature
		 */
	case class Feature(id: Int, a: Double, b: Double) {
		lazy val x: DblArray = Array[Double](a, b)
	}
	
	private val indexedFeatures: Vector[(Feature, Int)] = Vector.tabulate(120)(n => {
		val id = Random.nextInt(K)
		(Feature(n, fGen(id+1), fGen(id+1)), id)
	})

	private val MAX_ITERS = 250

		/**
		 * Execution of the scalatest for '''KMeans''' class
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate
		 * 	
		 * Exceptions thrown during the execution of the tests are caught by the wrapper or handler
		 * test method in Eval trait defined as follows:
		 * {{{
		 *    def test(args: Array[String]) =
		 *      Try(run(args)) match {
		 *        case Success(n) => ...
		 *        case Failure(e) => ...
		 * }}}
		 * The tests can be executed through ''sbt run'' or individually by calling 
		 * ''TestName.test(args)'' (i.e. DKalmanEval.test(Array[String]("IBM") )
		 * @param args array of arguments used in the test
		 */
	@throws(classOf[IllegalArgumentException])
	override protected def run(args: Array[String]): Int = {
		import scala.language.postfixOps
		
			// Create an ordered list of K groups of features sorted by their id
		val expected = indexedFeatures.groupBy( _._2).values.toList
					.sortWith(_(0)._2 < _(0)._2).map(_.sortWith(_._1.id < _._1.id))

		val expectedStr = toString(expected)
		show(s"\n$expectedStr")
		val features = indexedFeatures.unzip._1
	
		val kmeans = KMeans[Double](KMeansConfig(K, MAX_ITERS), features.map(_.x))
				
		kmeans.model match {
			case Some(m) =>
					// sort the model or list of clusters by the sum of features of their centroid
				val sorted = m.sortWith( _.center.sum < _.center.sum)
				
					// Retrieve the membership for each cluster
				val memberShip = sorted.map(_.getMembers)
				
					// Extract the id of expected features for each cluster
				val expectedId = expected.map( _.map( _._1.id))
				
				memberShip.zip(expectedId).foreach{ case (c, e) => assertList(c, e.toList)}
				show(s"members\n${memberShip.map(_.mkString(", ")).mkString("\n")}")

			case None => error("Failed building a K-means model")
		}
	}
	
	private def toString(groups: Iterable[Vector[Any]]): String = {
		groups.map( _.map(_.toString).mkString("\n")).mkString("\n-------\n")
	}
}


// -----------------------------------  EOF ---------------------------------------------------
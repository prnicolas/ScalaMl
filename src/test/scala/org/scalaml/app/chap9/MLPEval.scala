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
package org.scalaml.app.chap9

import org.scalaml.workflow.data.DataSource
import org.scalaml.supervised.nnet.{MLPConfig, MLP}
import org.scalaml.trading.GoogleFinancials
import org.scalaml.core.Types.ScalaMl.{DblArray, DblMatrix, DblVector}
import org.scalaml.util.{DisplayUtils,  LoggingUtils}
import org.scalaml.trading.GoogleFinancials._
import org.scalaml.app.Eval
import LoggingUtils._


		/**
		 * '''Purpose:'''Singleton to evaluate the Multi-layer perceptron by classifying
		 * Exchange Traded funds (ETF)
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning Chapter 9: Artificial Neural Network / Evaluation
		 */
object MLPEval extends Eval {
	import org.apache.log4j.Logger
	import scala.util.{Try, Success, Failure}
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "MLPEval"
    
	private val path = "resources/data/chap9/"
	private val ALPHA = 0.8
	private val ETA = 0.05
	private val NUM_EPOCHS = 2500
	private val EPS = 1e-6
	private val THRESHOLD = 0.25

	private val symbols = Array[String](
		"FXE", "FXA", "SPY", "GLD", "FXB", "FXF", "FXC", "FXY", "CYB"
	)

	private val STUDIES = List[Array[String]](
		Array[String]("FXY", "FXC", "GLD", "FXA"),
		Array[String]("FXE", "FXF", "FXB", "CYB"),
		Array[String]("FXE", "FXC", "GLD", "FXA", "FXY", "FXB"),
		Array[String]("FXC", "FXY", "FXA"),
		Array[String]("CYB", "GLD", "FXY"),
		symbols	
	)
  
	implicit val mode = new MLP.MLPBinClassifier
  		/**
		 * Execution of the scalatest for '''MLP''' class 
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
		 * 
		 * @param args array of arguments used in the test
		 */
	override protected def run(args: Array[String]): Int = {
		require(args.length > 0, "MLPEval.run Should have at least one argument")
		show(s"$header MLP evaluation")
	  
		val hiddenLayers = args.map( _.toInt)
		
		val desc = s"""MLP classifier without SoftMax conversion
			| ${args.mkString(" ")} hidden layers""".stripMargin
		show( desc)
		
		val prices = symbols.map(s => DataSource(s"$path$s.csv", true, true, 1))
				.flatMap(_.get(close).toOption)
			
		show(s"Data input size: ${prices(0).length}")
		test(hiddenLayers, prices)
	}
  

	private def test(hiddenLayers: Array[Int], prices: Array[DblVector]): Int = {
		show(s"${hiddenLayers.size} layers:(${hiddenLayers.mkString(" ")})")
  
		val startTime = System.currentTimeMillis
		val config = MLPConfig(ALPHA, ETA, NUM_EPOCHS, EPS)
		
		STUDIES.foreach( eval(hiddenLayers, prices, config, _))
		show(s"Duration ${System.currentTimeMillis - startTime} msecs.")
	}

	private def eval(
	    hiddenLayers: Array[Int],
			obs: Array[DblVector], 
			config: MLPConfig, 
			etfsSet: Array[String]): Int = 
		fit(hiddenLayers, etfsSet, obs, config).map(acc => show(s"Accuracy: $acc"))
			.getOrElse(error(s"$name could not compute the accuracy"))

	private def fit(
			hiddenLayers: Array[Int],
			symbols: Array[String], 
			prices: Array[DblVector], 
			config: MLPConfig): Option[Double] = {  
 
		val obs = symbols.flatMap( index.get(_)).map( prices( _ ).toArray )

		val xv = obs.drop(1).transpose
		val expected = Array[DblArray](obs.head).transpose

		val classifier = MLP[Double](config, hiddenLayers, xv, expected)
	
		classifier.fit(THRESHOLD)
	}
  
    
	private val index: Map[String, Int] = {
		import scala.collection.mutable.HashMap
  		 
		val _index = new HashMap[String, Int]
		symbols.zipWithIndex./:(new HashMap[String, Int])((mp, si) 
				=> {mp.put(si._1, si._2); mp}).toMap
	}

	private def toString(symbols: Array[String]): String = 
			s"${symbols.drop(1).mkString(" ")} s => ${symbols(0)}"
}


// -------------------------------------  EOF ----------------------------------------------------------
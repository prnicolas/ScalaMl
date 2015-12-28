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

import scala.util.{Try, Success, Failure, Random}
import scala.reflect.ClassTag
import scala.collection._

import org.apache.log4j.Logger

import org.scalaml.core.Types.{ScalaMl, emptyString}
import org.scalaml.supervised.nnet.{MLPConfig, MLP}
import org.scalaml.stats.XTSeries._
import org.scalaml.util.{FormatUtils, DisplayUtils, MathUtils}
import org.scalaml.app.Eval
import DisplayUtils._, ScalaMl._, FormatUtils._, MathUtils._
import org.scalaml.plots.{LinePlot, Legend, LightPlotTheme}

		/**
		 * '''Purpose:'''Singleton to evaluate the binary (2 class) multi-layer perceptron.
		 * 
		 *  @author Patrick Nicolas
		 *  @see Scala for Machine Learning Chapter 9: ''Artificial Neural Network'' / Evaluation / 
		 *  Test case
		 */
object BinaryMLPEval extends Eval {

		/**
		 * Name of the evaluation 
		 */
	val name: String = "BinaryMLPEval"
	
	private val ALPHA = 0.3
	private val ETA = 0.03
	private val HIDDEN = Array[Int](7, 3)
	private val NUM_EPOCHS = 2000
	private val TEST_SIZE: Int = 100
	private val EPS = 1e-7


		/** Execution of the scalatest for '''MLP''' class.
		 * This method is invoked by the actor-based test framework function, ScalaMlTest.evaluate
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
		override protected def run(args: Array[String]): Int =  {
		import scala.language.postfixOps
  	  
		show(s"$header Binary Multi-layer perceptron")


		def f1(x: Double): DblArray = 
				Array[Double](0.1+ 0.5*Random.nextDouble, 0.6*Random.nextDouble)
		def f2(x: Double): DblArray = 
				Array[Double](0.6 + 0.4*Random.nextDouble, 1.0 - 0.5*Random.nextDouble)

		
			// Generate the synthetic time series of features
  	val HALF_TEST_SIZE = (TEST_SIZE>>1)
		val xt = Vector.tabulate(TEST_SIZE)(n => 
				if( n < HALF_TEST_SIZE) f1(n) else f2(n - HALF_TEST_SIZE))

		//	val xt = normalize(xv).get
			
			println(s"Input data ${xt.map( _.mkString(",")).mkString("\n")}")
		
			// Generate the synthetic expected values (labels)
		val yt = Vector.tabulate(TEST_SIZE)(n => 
				if( n < HALF_TEST_SIZE) Array[Double](0.0) else Array[Double](1.0) )
		
		val etaValues = List[Double](0.01, 0.02, 0.03, 0.1)
		val data = etaValues.flatMap( testEta(_, xt, yt)).map{ case(x, s) => (x.toVector, s) }
		val legend = new Legend("Err", "MLP [2-7-3-1] training - learning rate", "Epochs", "Error")
				
		LinePlot.display(data, legend, new LightPlotTheme)
	
		val alphaValues =  List[Double](0.0, 0.2, 0.4, 0.6)
		val data2 = alphaValues.flatMap( testAlpha(_, xt, yt)).map{ case(x, s) => (x.toVector, s) }
		val legend2 = new Legend("Err", "MLP [2-7-3-1] training - momentum", "Epochs", "Error")

		if( LinePlot.display(data2, legend2, new LightPlotTheme) ) 1 else -1
	}
	
	private def testEta(
			eta: Double, 
			xt: XVSeries[Double], 
			yt: XVSeries[Double]): Option[(mutable.ArrayBuffer[Double], String)] = {
	  		implicit val mlpObjective = new MLP.MLPBinClassifier
	  val config = MLPConfig(ALPHA, eta, NUM_EPOCHS, EPS)
		MLP[Double](config, HIDDEN, xt, yt).counters("err").map( (_, s"eta=$eta"))
	}
	
	private def testAlpha(
			alpha: Double, 
			xt: XVSeries[Double], 
			yt: XVSeries[Double]): Option[(mutable.ArrayBuffer[Double], String)] = {
	  		implicit val mlpObjective = new MLP.MLPBinClassifier
	  val config = MLPConfig(alpha, ETA, NUM_EPOCHS, EPS)
		MLP[Double](config, HIDDEN, xt, yt).counters("err").map( (_, s"alpha=$alpha"))
	}
}

// --------------------------------- EOF -----------------------------
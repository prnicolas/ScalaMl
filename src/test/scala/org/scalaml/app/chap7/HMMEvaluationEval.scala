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
package org.scalaml.app.chap7

import scala.util.{Try, Success, Failure}
import scala.language.implicitConversions
import scala.reflect.ClassTag
import org.apache.log4j.Logger


import org.scalaml.util.DisplayUtils
import org.scalaml.supervised.hmm.{HMM, HMMModel, HMMConfig, EVALUATION}
import org.scalaml.core.Types.ScalaMl.{DblArray, DblMatrix, XVSeries}
import org.scalaml.stats.XTSeries._
import org.scalaml.util.MathUtils._
import org.scalaml.app.Eval
import DisplayUtils._, HMM._

		/**
		 * '''Purpose:''' Singleton for the evaluation of Hidden Markov Models presented in 
		 * chapter 7. The purpose of the test is to compute the likelihood for a sequence of
		 * observed states (or input data) given a Lambda model.
		 * 
		 * @author Patrick Nicolas
		 * @version 0.98.2
		 * @see Scala for Machine Learning Chapter 7 ''Sequential data models'' / Hidden Markov model
		 */
object HMMEvaluationEval extends Eval  {
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "HMMEvaluationEval2"
	final val EPS = 1e-2

			// Step 1. Defined the Lambda model
	  
		// State-transition probabilities matrix for HMM
		val A0 = Array[Array[Double]](
			Array[Double](0.21, 0.13, 0.25, 0.06, 0.11, 0.24),
			Array[Double](0.31, 0.17, 0.18, 0.04, 0.19, 0.11),
			Array[Double](0.32, 0.15, 0.15, 0.01, 0.21, 0.16),
			Array[Double](0.25, 0.12, 0.11, 0.01, 0.27, 0.24),
			Array[Double](0.22, 0.10, 0.09, 0.03, 0.31, 0.25),
			Array[Double](0.15, 0.08, 0.05, 0.07, 0.43, 0.22)
		)
  	  		
		// Emission/observations probabilities matrix
		val B0 =  Array[Array[Double]](
			Array[Double](0.45, 0.12, 0.43),
			Array[Double](0.54, 0.26, 0.20),
			Array[Double](0.25, 0.51, 0.24),
			Array[Double](0.46, 0.33, 0.21),
			Array[Double](0.33, 0.57, 0.10),
			Array[Double](0.42, 0.39, 0.19)
		)

		val PI0 = Array[Double](0.26, 0.04, 0.11, 0.26, 0.19, 0.14)
		

		/** Execution of the scalatest for '''HMM''' class.
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
	override protected def run(args: Array[String]): Int =  {
		import scala.language.postfixOps
		import HMMModel._
	  
		show(s"$header Hidden Markov Model - Evaluation")
		if( !validate(A0, B0, PI0))
			throw new IllegalStateException("HMMEvaluationEval incorrect lambda model")
		
		show("Lambda model validated")
		// Step 2: Defined the sequence of observed states	
		val data = Vector[Double](
			0.0, 1.0, 2.0, 1.0, 3.0, 0.0, 1.0, 2.0, 2.0, 1.0, 3.0, 0.0, 1.0, 0.0, 2.0, 1.0
		)

		// The quantization method consists of normalizing the input data over the
			// number (or range) of symbols associated to the lambda model. The number of symbols
			// is the size of the rows of the emission probabilities matrix B
		val max = data.max
		val min = data.min
		implicit val quantize = (x: DblArray) => ((x.head/(max - min) + min)*(B0.head.length-1)).toInt

			// Step 3: Create a model 
		val xt: XVSeries[Double] = data.map(Array[Double](_))
		val lambda = HMMModel(DMatrix(A0), DMatrix(B0), PI0, data.length)
		
			// Step 4: Compute the likelihood of the sequence of observations
		
			// Make sure the values in the input lambda model are normalized
		if( lambda.validate(EPS) )
			// Invokes the evaluation form for the lambda model
			evaluate(lambda, xt).map( _.toString).map( show(_))
				.getOrElse(error("HMM evaluation failed"))
		else
			error("Lambda model for evaluation is not properly formed")
	}
}


// --------------------------------  EOF -------------------------------
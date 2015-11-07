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
package org.scalaml.app.chap8

import org.scalaml.supervised.svm.{SVMConfig, SVM}
import org.scalaml.supervised.svm.formulation._
import org.scalaml.supervised.svm.kernel._
import org.scalaml.stats.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.util.{DisplayUtils,  LoggingUtils}
import org.scalaml.app.Eval
import LoggingUtils._


		/**
		 * Singleton to evaluate the impact of different Kernel functions
		 * on the accuracy of the classification of a binary support vector
		 * classifier using synthetic features. The synthetic values are generated
		 * using a combination of random generators. 
		 * 
		 * The test compares kernel function by compute the percentage of true positives. A
		 * better alternative would be to compute the recall, precision and F1-score
		 * 
		 * @author Patrick Nicolas
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 8 ''Kernel Models and Support Vector Machines''
		 * @see libsvm
		 */
object SVCKernelEval extends Eval {
	import scala.util.{Random, Try}
	import org.apache.log4j.Logger
	import ScalaMl._, XTSeries._
		/**
		 * Name of the evaluation 
		 */
	val name: String = "SVCKernelEval"
	
		// Generic parameters for the support vector machines
	private val EPS = 0.001
	private val C = 1.0
	private val GAMMA = 0.8
	private val N = 100
	private val COEF0 = 0.5
	private val DEGREE = 2
	type Features = Vector[DblArray]


		/** Execution of the scalatest for '''SVC''' class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate
		 * 
		 * The method compare the impact of different kernel function
		 * using a synthetic features with format x = a*random + b*random
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
	override protected def run(args: Array[String]): Int = {
		show(s"$header Comparison of kernel functions for Support Vector Classifier")
	
		compareKernel(0.6, 0.3)
		compareKernel(0.7, 0.3)
		compareKernel(0.8, 0.3)
		compareKernel(1.3, 0.3)
	}

		/**
		 * Generator of synthetic data to evaluate the impact of kernel functions
		 */
	private def genData(variance: Double, mean: Double): Vector[DblArray] = {
		val rGen = new Random(System.currentTimeMillis)
		
		Vector.tabulate(N)(_ => {
			rGen.setSeed(rGen.nextLong)
			Array[Double](rGen.nextDouble, rGen.nextDouble).map(variance*_ - mean) 
		})
 	}
		
		// Computes the accuracy of the classifier on a synthetic test sets to
		// evaluate and compare multiple kernel functions.
	
	private def compareKernel(a: Double, b: Double): Int = {
		require(a > 0.0 && a < 2.0, s"Cannot compare Kernel with inadequate features a = $a")
		require(b > -0.5 && b < 0.5, s"Cannot compare Kernel with inadequate features b = $b")

			// We make sure that the synthetic training and validation set are extracted from
			// the same dataset, sharing the same data distribution, genData
		val trainingSet = genData(a, b) ++ genData(a, 1-b)
		val testSet = genData(a, b) ++ genData(a, 1-b)
		val setHalfSize = trainingSet.size>>1
	
		val legend = s"\nSVM: Skewed Random values: a=$a b= $b"
		val validationSet = trainingSet.drop(setHalfSize)
		display(legend, 
				trainingSet.take(setHalfSize).map(z => (z(0), z(1))), 
				validationSet.map(z => (z(0), z(1)))
		)
		
		val labels: DblVector = Vector.fill(N)(0.0) ++ Vector.fill(N)(1.0)
	
		val accuracies = accuracy(trainingSet, testSet, labels, new RbfKernel(GAMMA)) ::
			accuracy(trainingSet, testSet, labels, new SigmoidKernel(GAMMA)) ::
			accuracy(trainingSet, testSet, labels, LinearKernel) ::
			accuracy(trainingSet, testSet, labels, new PolynomialKernel(GAMMA, COEF0, DEGREE)) ::
						List[Double]()
						
		val kernelType = List[String](
				s"RBF($GAMMA)", s"Sigmoid($GAMMA)", "Linear", s"Polynomial($GAMMA, $COEF0, $DEGREE)"
		)
		val result = accuracies.zip(kernelType).map{case(ac, ks) => s"$ks\t\t$ac}" }.mkString("\n")
		

		show(s"\nComparison of kernel functions on synthetic data $legend\n$result")
	}
	
	
		/**
		 * Compute the accuracy of the SVM classifier given a kernel function.
		 * The accuracy is computed as the percentage of true positives
		 */
	private def accuracy(
			xt: Vector[DblArray], 
			test: Vector[DblArray], 
			labels: DblVector, 
			kF: SVMKernel): Double = {
		
		import scala.language.postfixOps
	 
			// Configuration and instantiation (training) of a support vector machine classifier
		val config = SVMConfig(new CSVCFormulation(C), kF)
		val svc = SVM[Double](config, xt, labels)
		
			// Retrieve the predictive partial function 
		val pfnSvc = svc |>
		
			// Count the true positives
		test.zip(labels).count{ case(x, y) => 
			if(pfnSvc.isDefinedAt(x)) pfnSvc(x).get == y else false}.toDouble/test.size
	 }

	private def display(label: String, xy1: Vector[DblPair], xy2: Vector[DblPair]): Boolean = {
		import org.scalaml.plots.{ScatterPlot, BlackPlotTheme, Legend}
		val labels = Legend(
			name, s"SVM Kernel evaluation", label, "Y"
		)
		ScatterPlot.display(xy1, xy2, labels, new BlackPlotTheme)
	}
}

// --------------------------- EOF --------------------------------------------------
/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.app.chap8

import org.scalaml.supervised.svm.{SVMConfig, SVM}
import org.scalaml.supervised.svm.formulation._
import org.scalaml.supervised.svm.kernel._
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.util.DisplayUtils
import org.scalaml.app.Eval


		/**
		 * <p>Singleton to evaluate the impact of different Kernel functions
		 * on the accuracy of the classification of a binary support vector
		 * classifier using synthetic features. The synthetic values are generated
		 * using a combination of random generators. </p>
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning Chapter 8 Kernel models and support vector machines.
		 */
object SVCKernelEval extends Eval {
	import scala.util.{Random, Try, Success, Failure}
	import org.apache.log4j.Logger
	import ScalaMl._, XTSeries._
		/**
		 * Name of the evaluation 
		 */
	val name: String = "SVCKernelEval"
	
	private val EPS = 0.001
	private val C = 1.0
	private val GAMMA = 0.8
	private val N = 100
	private val COEF0 = 0.5
	private val DEGREE = 2
	type Features = XTSeries[DblVector]


		/** <p>Execution of the scalatest for <b>SVC</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate<br>
		 * The method compare the impact of different kernel function
		 * using a synthetic features with format x = a*random + b*random</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		DisplayUtils.show(s"$header Comparison of kernel functions for Support Vector Classifier", 
				logger)
		Try {
			compareKernel(0.6, 0.3)
			compareKernel(0.7, 0.3)
			compareKernel(0.8, 0.3)
			compareKernel(1.3, 0.3)
		} match {
			case Success(n) => n
			case Failure(e) =>failureHandler(e)
		}
	}
    
	private def genData(variance: Double, mean: Double): DblMatrix = {
		val rGen = new Random(System.currentTimeMillis)
		Array.tabulate(N)( _ => {
			rGen.setSeed(rGen.nextLong)
			Array[Double](rGen.nextDouble, rGen.nextDouble).map(x => variance*x - mean) 
		})
 	}
		
	private def compareKernel(a: Double, b: Double): Int = {
		require(a > 0.0 && a < 2.0, s"Cannot compare Kernel with inadequate features a = $a")
		require(b > -0.5 && b < 0.5, s"Cannot compare Kernel with inadequate features b = $b")
    	   	
		val trainingSet = genData(a, b) ++ genData(a, 1-b)
		val testSet = genData(a, b) ++ genData(a, 1-b)
		val setHalfSize = trainingSet.size>>1
		
		val legend = s"\nSkewed Random values: a=$a b= $b"
		display(legend, trainingSet.take(setHalfSize)
									.map(z => (z(0), z(1))), trainingSet.drop(setHalfSize).map(z => (z(0), z(1))))
		
		val labels: DblVector = Array.fill(N)(0.0) ++ Array.fill(N)(1.0)
	       
		val results = 	evalKernel(trainingSet, testSet, labels, new RbfKernel(GAMMA)) ::
						evalKernel(trainingSet, testSet, labels, new SigmoidKernel(GAMMA)) ::
						evalKernel(trainingSet, testSet, labels, LinearKernel) ::
						evalKernel(trainingSet, testSet, labels, new PolynomialKernel(GAMMA, COEF0, DEGREE)) ::
						List[Double]()
						
		DisplayUtils.show(s"$name $legend completed", logger)
	}
	
	
	private def evalKernel(
			features: DblMatrix, 
			test: DblMatrix, 
			labels: DblVector, 
			kF: SVMKernel): Double = {
	  
		val config = SVMConfig(new CSVCFormulation(C), kF)
		val xt = XTSeries[DblVector](features)
		val svc = SVM[Double](config,  xt, labels)
		
		val successes = test.zip(labels)
							.count(tl => {
								Try((svc |> tl._1) == tl._2) match { 
									case Success(n) => true 
									case Failure(e) => false 
								}
							})
		successes.toDouble/test.size
	 }

	private def display(label: String, xy1: XYTSeries, xy2: XYTSeries): Unit = {
		import org.scalaml.plots.{ScatterPlot, BlackPlotTheme}
		val labels = List[String](
			name, s"SVC Kernel evaluation set", label, "Y"
		)
		ScatterPlot.display(xy1, xy2, labels, new BlackPlotTheme)
	}
}


// --------------------------- EOF --------------------------------------------------
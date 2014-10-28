/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.94
 */
package org.scalaml.app.chap8

import org.scalaml.supervised.svm._
import org.scalaml.core.XTSeries
import org.scalaml.core.types.ScalaMl._
import XTSeries._
import org.scalaml.plots.ScatterPlot
import org.scalaml.plots.BlackPlotTheme
import org.apache.log4j.Logger
import org.scalaml.util.Display
import scala.util.{Try, Success, Failure}


		/**
		 * <p>Singleton to evaluate the impact of different Kernel functions
		 * on the accuracy of the classification of a binary support vector
		 * classifier using synthetic features. The synthetic values are generated
		 * from </p>
		 * 
		 * @author Patrick Nicolas
		 * @since April 24, 2014
		 * @note Scala for Machine Learning
		 */
object SVCKernelEval {
	import scala.util.Random
    final val EPS = 0.0001
    final val C = 1.0
    final val GAMMA = 0.8
    final val N = 100
    final val COEF0 = 0.5
    final val DEGREE = 2
    type Features = XTSeries[DblVector]
    
    private val logger = Logger.getLogger("SVCKernelEval")
    	/**
    	 * Main routine to compare the impact of different kernel function
    	 * using a synthetic features with format x = a*random + b*random
    	 */
    def run: Unit = {
	    Display.show("Comparison of kernel functions for a Binary Support Vector Classifier", logger)
		compareKernel(0.6, 0.3)
	    compareKernel(0.7, 0.3)
	    compareKernel(0.8, 0.3)
	    compareKernel(1.4, 0.3)
	}
    
    
    private def genData(variance: Double, mean: Double): DblMatrix = 
		Array.fill(N)(Array[Double](variance*Random.nextDouble - mean, variance*Random.nextDouble - mean))

		
	private def compareKernel(a: Double, b: Double) {
    	require(a > 0.0 && a < 2.0, "Cannot compare Kernel with inadequate features a = " + a)
    	require(b > -0.5 && b < 0.5, "Cannot compare Kernel with inadequate features b = " + b)
    	   	
		val trainingSet = genData(a, b) ++ genData(a, 1-b)
		
		val testSet = genData(a, b) ++ genData(a, 1-b)
		val setHalfSize = trainingSet.size>>1
		
		val legend = new StringBuilder("\nSkewed Random values: a=").append(a).append(" b=").append(b).toString
		display(legend, trainingSet.take(setHalfSize).map(z => (z(0), z(1))), trainingSet.drop(setHalfSize).map(z => (z(0), z(1))))
		val labels = Array.fill(N)(0.0) ++ Array.fill(N)(1.0)
	       
        val results = evalKernel(trainingSet, testSet, labels, RbfKernel(GAMMA)) ::
                      evalKernel(trainingSet, testSet, labels, SigmoidKernel(GAMMA)) ::
                      evalKernel(trainingSet, testSet, labels, LinearKernel) ::
                      evalKernel(trainingSet, testSet, labels, PolynomialKernel(GAMMA, COEF0, DEGREE)) ::
                      List[Double]()
         Display.show(legend + "\n" + results.toString, logger)
	}
	
	private def evalKernel(features: DblMatrix, test: DblMatrix, labels: DblVector, kF: SVMKernel): Double = {
		val config = SVMConfig(new CSVCFormulation(C), kF)
	    val xt = XTSeries[DblVector](features)
		val svc = SVM[Double](config,  xt, labels)
		val successes = test.zip(labels)
		                    .count(tl => {
		                    	Try((svc |> tl._1) == tl._2)
		                    	match { 
		                    	  case Success(n) => true 
		                    	  case Failure(e) => false }})
		successes.toDouble/test.size
	 }

	private def display(label: String, xy1: XYTSeries, xy2: XYTSeries): Unit = {
       require(xy1 != null && xy1.size > 0, "Cannot display an undefined time series")
       
       val plotter = new ScatterPlot(("Training set", label, "Y"), new BlackPlotTheme)
       plotter.display(xy1, xy2, 250, 340)
	}
}



// --------------------------- EOF --------------------------------------------------
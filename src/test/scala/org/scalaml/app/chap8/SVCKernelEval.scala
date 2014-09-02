/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.app.chap8

import org.scalaml.supervised.svm._
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import XTSeries._
import org.scalaml.plots.ScatterPlot
import org.scalaml.plots.BlackPlotTheme



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
    
    
    	/**
    	 * Main routine to compare the impact of different kernel function
    	 * using a synthetic features with format x = a*random + b*random
    	 */
    def run: Unit = {
	    Console.println("Comparison of kernel functions for a Binary Support Vector Classifier")
		compareKernel(0.27, 0.23)
		compareKernel(0.16, 0.45)
		compareKernel(0.12, 0.19)
	}
    
    
    private def genLabels(a: Double, b: Double): DblMatrix = 
		Array.fill(N)(Array[Double](a*Random.nextDouble, b*Random.nextDouble))

	private def genFeatures(a: Double, b: Double): DblMatrix = 
		genLabels(a,b) ++
		Array.fill(N)(Array[Double](a*1.85*Random.nextDouble, 1.7*b*Random.nextDouble))

		
	private def compareKernel(a: Double, b: Double) {
    	require(a > 0.0 && a < 10.0, "Cannot compare Kernel with inadequate features a = " + a)
    	require(b > 0.0 && b < 10.0, "Cannot compare Kernel with inadequate features b = " + b)
    	   	
		val trainingSet = genLabels(a, b) 
		val setHalfSize = trainingSet.size>>1
		
		val legend = new StringBuilder("Skewed Random values: a=").append(a).append(" b=").append(b).toString
		display(legend, trainingSet.take(setHalfSize).map(z => (z(0), z(1))), trainingSet.drop(setHalfSize).map(z => (z(0), z(1))))
		val y = Array.fill(N)(0.0) ++ Array.fill(N)(1.0)
	       
		println(legend)
	    var testSet = genLabels(0.18, 0.35)
        evalKernel(trainingSet, testSet, y, RbfKernel(GAMMA))  
        evalKernel(trainingSet, testSet, y, SigmoidKernel(GAMMA))
        evalKernel(trainingSet, testSet, y, LinearKernel)
        evalKernel(trainingSet, testSet, y, PolynomialKernel(GAMMA, COEF0, DEGREE))
        
        testSet = genLabels(0.25, 0.6)
        evalKernel(trainingSet, testSet, y, RbfKernel(GAMMA))  
        evalKernel(trainingSet, testSet, y, SigmoidKernel(GAMMA))
        evalKernel(trainingSet, testSet, y, LinearKernel)
        evalKernel(trainingSet, testSet, y, PolynomialKernel(GAMMA, COEF0, DEGREE))
        
        testSet = genLabels(0.5, 0.5)
        evalKernel(trainingSet, testSet, y, RbfKernel(GAMMA))  
        evalKernel(trainingSet, testSet, y, SigmoidKernel(GAMMA))
        evalKernel(trainingSet, testSet, y, LinearKernel)
        evalKernel(trainingSet, testSet, y, PolynomialKernel(GAMMA, COEF0, DEGREE))
          
        testSet = genLabels(0.75, 0.45)
        evalKernel(trainingSet, testSet, y, RbfKernel(GAMMA))  
        evalKernel(trainingSet, testSet, y, SigmoidKernel(GAMMA))
        evalKernel(trainingSet, testSet, y, LinearKernel)
        evalKernel(trainingSet, testSet, y, PolynomialKernel(GAMMA, COEF0, DEGREE))
	}
	
	private def evalKernel(features: DblMatrix, eval: DblMatrix, lbl: DblVector, kF: SVMKernel): Unit = {
		val config = SVMConfig(CSVCFormulation(C), kF)
	    val ft = XTSeries[DblVector](features)
		val svc = SVM[Double](config,  ft, lbl)
		var vote = Array[Int](0,0)
		
		eval.zipWithIndex.foreach(zi => svc |> zi._1 match {
		   case Some(z) => if(z == lbl(zi._2)) vote(1) += 1 else vote(0) += 1
		   case None => println("Could not train SVM")
		})
	    println(vote(1) + "," + vote(0))
	}

	private def display(label: String, xy1: XYTSeries, xy2: XYTSeries): Unit = {
       require(xy1 != null && xy1.size > 0, "Cannot display an undefined time series")
       
       val plotter = new ScatterPlot(("Training set", label, "Y"), new BlackPlotTheme)
       plotter.display(xy1, xy2, 250, 340)
	}
}



// --------------------------- EOF --------------------------------------------------
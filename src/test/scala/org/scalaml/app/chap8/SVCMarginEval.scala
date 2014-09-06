/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.app.chap8

import org.scalaml.supervised.svm.{SVMConfig, RbfKernel, CSVCFormulation, SVMExecution, SVM}
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import XTSeries._
import org.scalaml.plots.ScatterPlot
import org.scalaml.plots.BlackPlotTheme



		/**
		 * <p>Singleton to evaluate the impact of the C penalty factor
		 * on the size of the margin in the training of a binary 
		 * support vector classifier using synthetic features.</p>
		 * 
		 * @author Patrick Nicolsa
		 * @since April 23, 2014
		 * #project Scala for Machine Learning
		 */
import XTSeries._
object SVCMarginEval {
	import scala.util.Random
    final val GAMMA = 0.8; val N = 100
    
    	/**
    	 * Main evaluation routine that consists of two steps:<br>
    	 * Generation of synthetic features<br>
    	 * Computation of the margin for a specific C penalty value
    	 */
	def run: Unit = {   
        Console.println("Evaluation of impact of C penalty on margin of a binary support vector classifier")
    	val values = generate
		Range(0, 50).foreach( i => computeMargin(values._1, values._2, i*0.1))
	}
    
    private def generate: (DblMatrix, DblVector) = {
      val z  = Array.tabulate(N)(i =>{
	    val ri = i*(1.0 + 0.2*Random.nextDouble)	
	    Array[Double](i, ri)
	  }) ++
	  Array.tabulate(N)(i =>Array[Double](i, i*Random.nextDouble))
	  
      (z, Array.fill(N)(1) ++ Array.fill(N)(-1))
    }

	private def computeMargin(features: DblMatrix, lbl: DblVector, c: Double): Unit = {
	
		val config = SVMConfig(CSVCFormulation(c), RbfKernel(GAMMA))
		val svc = SVM[Double](config, XTSeries[DblVector](features),lbl)
		svc.margin match {
			case Some(mrgn) => 	println( "\nMargin: " + mrgn )
			case None => println("training failed")
		}
	}
}



// --------------------------- EOF --------------------------------------------------
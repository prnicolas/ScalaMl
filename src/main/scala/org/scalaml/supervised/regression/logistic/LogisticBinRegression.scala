/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95
 */
package org.scalaml.supervised.regression.logistic

import org.scalaml.core.types
import types.ScalaMl._
import org.scalaml.util.Display
import org.apache.log4j.Logger


	/**
	 * <p>Logistic regression for a binary (2-class) classifier. The number of weights (model) is defined
	 * by the number of classifiers + 1. The likelihood (conditional probability is computed as 
	 * 1/(1 + exp(-(w0 + w1.x1 + w2.x2)).<br>
	 * The training (extraction of the weights) is computed as part of the instantiation of the class so 
	 * the model is either complete or undefined so classification is never done on incomplete (or
	 * poorly trained) model (computation error, maximum number of iterations exceeded).</p>
	 * @param labels data used to train a model
	 * @param maxIters maximum number of iterations used during training
	 * @param eta slope used in the computation of the gradient
	 * @param eps convergence criteria used to exit the training loop
	 * @throws IllegalArgumentException if labels are undefined, or the values of maxIters, eta or eps are out of range
	 * @author Patrick Nicolas
	 * @since January 11, 2014
	 */
final class LogBinRegression(labels: DVector[(XY, Double)], maxIters: Int, eta: Double, eps: Double) {
	require(maxIters > 10 && maxIters < 10000, s"Maximum number of iteration $maxIters is out of bounds")
	require(eta < 1e-1 && eta > 1e-7, s"Gradient slope $eta + is out of bounds")
	require(eps < 0.25 && eps > 1e-7, s"Convergence criteria $eps is out of bounds")
	require(labels != null && labels.size > 1, "Cannot train with undefined set of observations")
	    	
	final val dim = 3
	private[this] val weights = train
	private val logger = Logger.getLogger("LogBinRegression")

		/**
		 * <p>classification of a two dimension data (xy) using a binary logistic regression.</p>
		 * @param xy (x,y) data point
		 * @return Option (class, likelihood) for the logistic regression is the training was completed, None otherwise
		 */
   	def classify(xy: XY): Option[(Boolean, Double)] = weights match {
	  case Some(w) => { 
	  	 val likelihood =  sigmoid(w(0) + xy._1*w(1) + xy._2*w(2))
	     Some(likelihood > 0.5, likelihood)
	   }
	   case None => Display.error("classify failed", logger); None
	}
	
	
		/*
		 * Implements the training algorithm for the logistic regression. The model (weights)
		 * is not initialized if the training loop does not converge before the maximum 
		 * number of iterations is reached.
		 */
	private[this] def train: Option[DblVector] = {
		import scala.util.Random    
    	val w = Array.tabulate(dim)( x=> Random.nextDouble-1.0) 
    	
    	(0 until maxIters).find( i => {
    	   val deltaW = labels.foldLeft(Array.fill(dim)(0.0)) ((dw, y) => {  
    	  	   val z = sigmoid(w(0) + w(1)*y._1._1 +  w(2)*y._1._2)
    	  	   dw.map(dx => dx + (y._2 - z)*(y._1._1 + y._1._2))
    	   })

    	   val nextW = Array.fill(dim)(0.0).zipWithIndex.map( nw => w(nw._2) + eta*deltaW(nw._2))
	       val diff = Math.abs(nextW.sum - w.sum)
	    	
	       nextW.copyToArray(w)
	       diff < eps
    	}) match {
    		case Some(iters) => Some(w)
    		case None => Display.error("LogBinRegression.train failed to converge", logger); None
    	}
	}
	
	@inline
	private[this] def sigmoid(x: Double): Double = 1.0/(1.0 + Math.exp(-x))

}


// ----------------------  EOF ------------------------------------
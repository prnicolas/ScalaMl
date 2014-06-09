/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.workflow.module



import org.scalaml.workflow.Transform
import org.apache.commons.math3.optimization.general.GaussNewtonOptimizer
import org.apache.commons.math3.optimization.fitting.PolynomialFitter
import scala.Array.canBuildFrom

import org.scalaml.core.Types

	/**
	 * Factory for the layout
	 */
/*
trait ClassifierModule[T] {
  val classifier: Classifier[T]
    
  import Types._
  abstract class Classifier[T](implicit val fc: T => DblVector) extends Transform[T, DblVector](fc) {
     def |> (data: T): Option[DblVector]
  }
  * 
  */
  
  /*
  class PolynomialFit[T](val degree: Int)(implicit val _fc: T => DblVector) extends Classifier[T] {
  	 val model = new Array[Double](degree+1)
  	 
  	 override def |> (data: T): Array[Double] =  {
  		if( model == null ) {
	  	  	val fitter = fc(data).foldLeft(new PolynomialFitter(degree, new GaussNewtonOptimizer))((fitter, x) => { 
	  	  	  fitter.addObservedPoint(x._1, x._2)
	  	  	  fitter 
	  	  	})
	  	  	model ++: fitter.fit
	  	  	model
  	    }
  		else {
  			fc(data) map { x => {val diff= x._2 -polynomial(x._1); diff } }
  		}
  	  }

  	  private[this] def polynomial(x: Double): Double = {
  	  	  var x0 = 1.0
  	  	  model.reduceLeft ((s, y) =>{ x0 *= x; s +  y*x  })
  	  }
  }
  * 
  */
  
  
  /*
  class LogisticRegression[T](val data: T, val size: Int, val maxIters: Int, val slope: Double)(implicit dot: (T, Array[Double]) => Array[Double]) extends Classifier[T](data) {
    import scala.util.Random
    
	private[this] var weights = new Array[Double](size) map { x => Random.nextDouble }	
	
   	
		/**
		 * Implements the training algorithm for the logistic regression. The observations are defined
		 * in the array as from index 1, to observations.size -1. The value in index 0 is the label
		 * 
		 * @observations array of array of floating point values
		 * @exception IllegalArgumentException if the observations set is undefined or its size < 2
		 */
	def train(labels: Array[Int], gradient: (T, Array[Double]) => Array[Double]): Unit = {
       	
		(0 until maxIters) foreach( n => {
			foreach( input=> { 
			   val dotProd = |> ((t: T) => dot(t, weights))
			   val logit = dotProd.reduceLeft((s,prod) => s + prod )
		       val predicted = sigmoid(logit)
		       
		       		 // Closure assume predicted and slope already defined!
			   weights = gradient(data, weights)
			})
		})
	}

		/**
		 * Classify a new observation with the logistic regression model. The size
		 * of the observation should be equal to the number of the features
		 * @param input observation to classify
		 * @exception IllegalArgumentException if the input has not the proper size
		 */
    override def |> : Double = logit
   

 
	private[this] def logit: Double = {
	   val dotProd = |> ((t: T) => dot(t, weights))
  	   val logit = dotProd.reduceLeft((s,prod) => s + prod )
	   sigmoid(logit)
	}
	
	@inline
	private[this] def sigmoid(x: Double): Double = 1.0/(1.0 + Math.exp(-x))
  }
  * 
  */
/*
import Types._
class PolynomialFit[T](val degree: Int)(implicit val _fc: T => DblVector) extends Classifier[T] {
    def |> (data: T): Option[DblVector] = None
}
* 
*/



object ClassifierModuleApp extends App {
	val values = Array[Double](0.0, 1.0, 4.0, 5.0, 7.0, 1.2, 12.8, 11.9, 4.8)
	val labels = Array[Int](1, 0, 1, 1, 1, 0, 1, 1, 0)
	val predicted = 1.0
	val slope = 0.002
	
	var k = 0;
	val gradient = (x: Array[Double], y: Array[Double]) => {
	   val z = x.zip(y)
	   z map( e => e._1 + slope*(labels(k) - predicted)*e._2 )
	}
	
	implicit val dot = (x: Array[Double], y: Array[Double]) => {
	  x.zip(y) map( x => x._1*x._2)
	}
		
}



// -----------------------  EOF -----------------------------------------------
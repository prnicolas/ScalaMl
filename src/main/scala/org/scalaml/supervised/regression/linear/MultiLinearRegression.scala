/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.supervised.regression.linear

import org.scalaml.core.{XTSeries, Types}
import Types.ScalaMl._
import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression
import org.apache.commons.math3.exception.{MathIllegalArgumentException, MathRuntimeException, OutOfRangeException}
import org.scalaml.workflow.PipeOperator
import org.scalaml.core.Types.CommonMath._
import scala.annotation.implicitNotFound



		/**
		 * <p>Class that defines a Multivariate linear regression. The regression
		 * coefficients or weights are computed during the instantiation of the 
		 * class. The computation of the regression coefficients uses the 
		 * Apache commons Math library. THe regression coefficients are
		 * initialized as None if the training fails.</p>
		 * @param xt input multi-dimensional time series for which regression is to be computed
		 * @param y labeled data for the Multivariate linear regression
		 * @exception IllegalArgumentException if the input time series or the labeled data are undefined or have different sizes
		 * 
		 * @author Patrick Nicolas
		 * @data April 19, 2014
		 * @project Scala for Machine Learning
		 */
@implicitNotFound("Implicit conversion of type to Double for MultiLinearRegression is missing")
final class MultiLinearRegression[@specialized(Double) T <% Double](val xt: XTSeries[Array[T]], val y: XTSeries[T]) 
                    extends OLSMultipleLinearRegression with PipeOperator[Array[T], Double] {
	
	require(xt != null && xt.size > 0, "Cannot create perform a Multivariate linear regression on undefined time series")
	require(y != null && y.size > 0, "Cannot train a Multivariate linear regression with undefined labels")
    require (xt.size == y.size, "Size of Input data " + xt.size + " and labels " + y.size + " for Multivariate linear regression are difference")
		
	val weights: Option[DblVector] = {
	  try {
		newXSampleData(xt.toDblMatrix)
		newYSampleData(y.toDblVector)
	 	Some(estimateRegressionParameters)
	  }
	  catch {
		 case e: MathIllegalArgumentException => println("Cannot compute regression coefficients: " + e.toString); None
		 case e: MathRuntimeException => println("Cannot compute regression coefficients: " + e.toString); None
		 case e: OutOfRangeException =>  println("Cannot compute regression coefficients: " + e.toString); None
	  }
	}

    
		/**
		 * <p>Data transformation that predicts the value of a vector input.</p>
		 * @param x Array of parameterized values
		 * @exception IllegalArgumentException if the input array is undefined
		 * @return predicted value if the model has been successfully trained, None otherwise
		 */
	override def |> (x: Array[T]): Option[Double] = {
	  require( x != null && x.size > 0, "Cannot predict undefined value using the Multivariate linear regression")
	  require( x.size != weights.size -1, "Size of input data for prediction " + x.size + " should be " + (weights.size -1))
	  
	  if( weights == None) None
	  else
		Some(x.zip(weights.get.drop(1)).foldLeft(weights.get(0))((s, z) => s + z._1*z._2))
	}
}



		/**
		 * <p>Companion object that defines the 
		 * constructor for the class MultiLinearRegression.</p>
		 * 
		 * @author Patrick Nicolas
		 */
object MultiLinearRegression {
	def apply[T <% Double](xt: XTSeries[Array[T]], y: XTSeries[T]): MultiLinearRegression[T] = new MultiLinearRegression[T](xt, y)
}

// ------------------------------  EOF ---------------------------------------
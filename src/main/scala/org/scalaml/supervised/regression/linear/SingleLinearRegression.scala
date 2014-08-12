/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.supervised.regression.linear

import org.scalaml.core.XTSeries
import org.apache.commons.math3.stat.regression.SimpleRegression
import org.scalaml.core.Types.ScalaMl.DblMatrix
import org.scalaml.workflow.PipeOperator
import scala.annotation.implicitNotFound

		/**
		 * <p>Class that defines the linear regression for a single variable. The model (w,r),
		 * (slope, intercept) is created during instantiation of the class to reduce the life-cycle
		 * of instances.</p>
		 * @param xt single parameterized variable time series
		 * @throws IllegalArgumentException if the time series is undefined
		 * @throws implicitNotFound if conversion from type to Double is not implicitely defined
		 * 
		 * @author Patrick Nicolas
		 * @since April 27, 2014
		 * @note Scala for Machine Learning
		 */
@implicitNotFound("Implicit conversion of type to Double for SingleLinearRegression is missing")
class SingleLinearRegression[@specialized(Double) T <% Double](val xt: XTSeries[T])(implicit g: Double => T)  
                                          extends PipeOperator[Int, T] {
	require(xt != null && xt.size > 0, "Cannot compute the single variate linear regression of a undefined time series")
	
    private val model: Option[(Double, Double)] = {
    	try {
	    	val data: DblMatrix = xt.zipWithIndex.map(x => Array[Double](x._2.toDouble, x._1.toDouble))
	    	val regr = new SimpleRegression(true)
	    	regr.addData(data)
	    	Some((regr.getSlope, regr.getIntercept))
    	}
    	catch {
    		case e: RuntimeException => println("Linear regression failed"); None
    	}
    }
	
	def slope: Option[Double] = model match {
		case Some(m) => Some(m._1)
		case None => None
	}
	
	def intercept: Option[Double] = model match {
	    case Some(m) => Some(m._2)
		case None => None
	}
    
    	/**
    	 * <p>Data transformation that computes the predictive value of a time series
    	 * using a single variable linear regression model. The model is initialized
    	 * during instantiation of a class.</p>
    	 * @param index of the time series n
    	 * @return new predictive value for index index
    	 */
    override def |> (index: Int): Option[T] = model match {
    	case None => None
    	case Some(m) => Some(m._1*index + m._2)
    }
}



object SingleLinearRegression {
	def apply[T <% Double](xt: XTSeries[T])(implicit g: Double => T): SingleLinearRegression[T] = new SingleLinearRegression[T](xt)
}
// ----------------------------------  EOF ----------------------------------------
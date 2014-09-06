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
package org.scalaml.supervised.regression.linear

import org.scalaml.core.XTSeries
import org.apache.commons.math3.stat.regression.SimpleRegression
import org.scalaml.core.Types.ScalaMl.DblMatrix
import org.scalaml.workflow.PipeOperator
import scala.annotation.implicitNotFound
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display

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
protected class SingleLinearRegression[T <% Double](val xt: XTSeries[(T, T)])(implicit g: Double => T)  
                                          extends PipeOperator[Double, T] {
	require(xt != null && xt.size > 0, "Cannot compute the single variate linear regression of a undefined time series")
	type XY = (Double, Double)
	private val logger = Logger.getLogger("SingleLinearRegression")
	
    private val model: Option[XY] = {
       Try {
	    //  val data: DblMatrix = xt.zipWithIndex.map(x => Array[Double](x._2.toDouble, x._1.toDouble))
      	  val data: DblMatrix = xt.toArray.map( x => Array[Double](x._1, x._2))
	      val regr = new SimpleRegression(true)
	      regr.addData(data)
	      (regr.getSlope, regr.getIntercept)
    	} match {
    		case Success(w) => Some(w)
    		case Failure(e) => Display.error("SingleLinearRegression ", logger,e); None
    	}
	}
	
	final def slope: Option[Double] = model match {
		case Some(m) => Some(m._1)
		case None => None
	}
	
	final def intercept: Option[Double] = model match {
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
    override def |> (x: Double): Option[T] = model match {
    	case None => None
    	case Some(m) => Some(m._1*x + m._2)
    }
}



		/**
		 * <p>Companion object for the single variable linear regression. This 
		 * singleton is used to define the constructor for the class SingleLinearRegression.</p>
		 * 
		 * @author Patrick Nicolas
		 */
object SingleLinearRegression {
	def apply[T <% Double](xt: XTSeries[(T, T)])(implicit g: Double => T): SingleLinearRegression[T] = new SingleLinearRegression[T](xt)
}
// ----------------------------------  EOF ----------------------------------------
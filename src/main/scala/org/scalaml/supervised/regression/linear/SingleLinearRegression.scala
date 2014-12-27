/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.supervised.regression.linear

// Scala classes
import scala.annotation.implicitNotFound
import scala.util.{Try, Success, Failure}

import org.apache.log4j.Logger
import org.scalaml.core.XTSeries
import org.apache.commons.math3.stat.regression.SimpleRegression

import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.Design.PipeOperator
import org.scalaml.util.DisplayUtils
import ScalaMl._

		/**
		 * <p>Class that defines the linear regression for a single variable. The model (w,r),
		 * (slope, intercept) is created during instantiation of the class to reduce the life-cycle
		 * of instances. The conversion of a Double back to a type T has to be defined prior 
		 * instantiating this class.
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;
		 * ,Arial,Helvetica,sans-serif;">
		 * regression:  w' = argmin Sum of squares {y(i)  - f(x(i)|w)}<br>
		 * with f(x|w) = w(0) + w(1).x</span></pre></p>
		 * @constructor Create a single linear regression model of type bounded to a Double as a view.
		 * @see org.apache.commons.math3.stat.regression._
		 * @throws IllegalArgumentException if the time series is undefined
		 * @throws implicitNotFound if conversion from type to Double is not implicitly defined
		 * @param xt Time series of (x,y) pairs of values
		 * @param g Implicit conversion from a <b>Double</b> to the type <b>T</b>
		 * 
		 * @author Patrick Nicolas
		 * @since April 27, 2014
		 * @note Scala for Machine Learning Chapter 6 Regression and regularization / One variate 
		 * linear regression
		 */
@implicitNotFound("Implicit conversion of type to Double for SingleLinearRegression is missing")
final class SingleLinearRegression[T <% Double](val xt: XTSeries[(T, T)])(implicit g: Double => T)  
				extends PipeOperator[Double, T] {
	
	require( !xt.isEmpty, 
			"SingleLinearRegression. Cannot create a linear regression with undefined time series")
	
	type XY = (Double, Double)
	private val logger = Logger.getLogger("SingleLinearRegression")
	
			// Create the model during instantiation. The model is 
			// actually create (!= None) if the regression coefficients can be computed.
	private[this] val model: Option[XY] = train match {
		case Success(w) => Some(w)
		case Failure(e) => DisplayUtils.none("SingleLinearRegression Model is undefined", logger,e)
	}
	
		/**
		 * <p>Retrieve the slope for this single variable linear regression.</p>
		 * @return slope of the linear regression if model has been properly trained, None otherwise
		 */
	final def slope: Option[Double] = model.map(_._1)
	
		/**
		 * <p>Retrieve the intercept for this single variable linear regression.</p>
		 * @return intercept of the linear regression if model has been properly trained, None otherwise
		 */
	final def intercept: Option[Double] = model.map(_._2)

		/**
		 * <p>Test if the model has been trained and is defined.</p>
		 * @return true is the model has been trained, false otherwise
		 */
	final def isModel = model != None
	
		/**
		 * <p>Data transformation that computes the predictive value of a time series
		 * using a single variable linear regression model. The model is initialized
		 * during instantiation of a class.</p>
		 * @throws MatchError if the regression model is undefined
		 * @return PartialFunction of a Double value as input and the value computed using the model 
		 * as output
		 */	
	override def |> : PartialFunction[Double, T] = {
			// Compute the linear function y = slope.x + intercept
		case x: Double if(model != None) => model.get._1*x + model.get._2
	}
	
		
	private def train: Try[XY] = {
		Try {
				// Convert the time series into a observations matrix
			val data: DblMatrix = xt.toArray.map( x => Array[Double](x._1, x._2))
				
				// Invoke Apache commons math library for the simple regression
			val regr = new SimpleRegression(true)
			regr.addData(data)
			
				// returns the slope and intercept from Apache commons math library
			(regr.getSlope, regr.getIntercept)
		} 
	}
}



		/**
		 * <p>Companion object for the single variable linear regression. This 
		 * singleton is used to define the constructor for the class SingleLinearRegression.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since April 27, 2014
		 * @note Scala for Machine Learning Chapter 6 Regression and regularization / One variate 
		 * linear regression
		 */
object SingleLinearRegression { 
		/**
		 * Default constructor for the SingleLinearRegression class
		 * @param xt Time series of (x,y) pairs of values
		 * @param g Implicit conversion from a <b>Double</b> to the type <b>T</b>
		 */
	def apply[T <% Double](xt: XTSeries[(T, T)])(implicit g: Double => T): SingleLinearRegression[T] = 
		new SingleLinearRegression[T](xt)
}
// ----------------------------------  EOF ----------------------------------------
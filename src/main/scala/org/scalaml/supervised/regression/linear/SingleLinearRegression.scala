/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.supervised.regression.linear

import org.scalaml.core.XTSeries
import org.apache.commons.math3.stat.regression.SimpleRegression
import org.scalaml.core.types.ScalaMl._
import org.scalaml.core.design.PipeOperator
import scala.annotation.implicitNotFound
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display

		/**
		 * <p>Class that defines the linear regression for a single variable. The model (w,r),
		 * (slope, intercept) is created during instantiation of the class to reduce the life-cycle
		 * of instances. The conversion of a Double back to a type T has to be defined prior instantiating this class<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
		 * <b>xt</b>   Time series of (x,y) pairs of values
		 * <b>g</b>    Implicit conversion from a <b>Double</b> to the type <b>T</b>
		 * </span></pre></p>
		 * @constructor Create a single linear regression model of type bounded to a Double as a view. 
		 * @throws IllegalArgumentException if the time series is undefined
		 * @throws implicitNotFound if conversion from type to Double is not implicitly defined
		 * 
		 * @author Patrick Nicolas
		 * @since April 27, 2014
		 * @note Scala for Machine Learning Chapter 6 Regression and regularization/One variate linear regression
		 */
@implicitNotFound("Implicit conversion of type to Double for SingleLinearRegression is missing")
final class SingleLinearRegression[T <% Double](val xt: XTSeries[(T, T)])(implicit g: Double => T)  
				extends PipeOperator[Double, T] {
	require(xt != null && xt.size > 0, "SingleLinearRegression. Cannot create a simple linear regression with undefined time series")
	
	type XY = (Double, Double)
	private val logger = Logger.getLogger("SingleLinearRegression")
	
			// Create the model during instantiation 
	private[this] val model: Option[XY] = {
		Try {
			val data: DblMatrix = xt.toArray.map( x => Array[Double](x._1, x._2))
			val regr = new SimpleRegression(true)
			regr.addData(data)
			(regr.getSlope, regr.getIntercept)
		} 
		match {
			case Success(w) => Some(w)
			case Failure(e) => Display.none("SingleLinearRegression Model is undefined", logger,e)
		}
	}
	
		/**
		 * <p>Retrieve the slope for this single variable linear regression.</p>
		 * @return slope of the linear regression if model has been properly trained, None otherwise
		 */
	final def slope: Option[Double] = model match {
		case Some(m) => Some(m._1)
		case None => Display.none("SingleLinearRegression.slope model is undefined", logger)
	}
	
		/**
		 * <p>Retrieve the intercept for this single variable linear regression.</p>
		 * @return intercept of the linear regression if model has been properly trained, None otherwise
		 */
	final def intercept: Option[Double] = model match {
		case Some(m) => Some(m._2)
		case None => Display.none("SingleLinearRegression.intecept model is undefined", logger)
	}
    
		/**
		 * <p>Data transformation that computes the predictive value of a time series
		 * using a single variable linear regression model. The model is initialized
		 * during instantiation of a class.</p>
		 * @throws MatchError if the regression model is undefined
		 * @return PartialFunction of a Double value as input and the value computed using the model as output
		 */	
	override def |> : PartialFunction[Double, T] = {
		case x: Double if(model != None) => model.get._1*x + model.get._2
	}
}



		/**
		 * <p>Companion object for the single variable linear regression. This 
		 * singleton is used to define the constructor for the class SingleLinearRegression.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since April 27, 2014
		 * @note Scala for Machine Learning Chapter 6 Regression and regularization/One variate linear regression
		 */
object SingleLinearRegression { 
	def apply[T <% Double](xt: XTSeries[(T, T)])(implicit g: Double => T): SingleLinearRegression[T] = 
		new SingleLinearRegression[T](xt)
}
// ----------------------------------  EOF ----------------------------------------
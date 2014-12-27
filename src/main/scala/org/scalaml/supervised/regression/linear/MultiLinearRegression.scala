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

import org.apache.log4j.Logger
import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression
import org.apache.commons.math3.exception.{MathIllegalArgumentException, MathRuntimeException, OutOfRangeException}

import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.Design.PipeOperator
import org.scalaml.core.Types.CommonMath
import org.scalaml.supervised.regression.RegressionModel
import org.scalaml.util.DisplayUtils
import ScalaMl._, CommonMath._


		/**
		 * <p>Class that defines a Multivariate linear regression. The computation of the regression 
		 * coefficients uses the Apache commons Math library. The regression model (regression 
		 * parameters or weights) are initialized only if the training was successful.<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
		 * Ordinary least squares regression:  w' = argmin Sum of squares {y(i)  - f(x(i)|w)}
		 * with regression model f(x|w) = w(0) + w(1).x(1) + ... + w(n).x(n)</span></pre></p>
		 * @constructor Creates multi-variate linear regression
		 * @throws IllegalArgumentException if the input time series or the labeled data are undefined 
		 * or have different sizes
		 * @param xt Input multi-dimensional time series for which regression is to be computed.
		 * @param y Labeled data for the Multivariate linear regression
		 * @see org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression
		 * @see org.scalaml.core.Design.PipeOperator
		 * 
		 * @author Patrick Nicolas
		 * @since April 19, 2014
		 * @note Scala for Machine Learning Chapter 6 Regression and regularization / Ordinary least 
		 * squares regression
		 */
final class MultiLinearRegression[T <% Double](xt: XTSeries[Array[T]], y: DblVector) 
					extends OLSMultipleLinearRegression with PipeOperator[Array[T], Double] {

	import scala.util.{Try, Success, Failure}
	import MultiLinearRegression._

	check(xt, y)
		// Parameterized features tpe
	type Feature = Array[T]
	private val logger = Logger.getLogger("MultiLinearRegression")
	
		/**
		 * The model is created using by OLSMultipleLinearRegression class of Apache
		 * commons math. The model is set to None if the regression weights cannot be computed.
		 */
	private[this] val model: Option[RegressionModel] = train match {
		case Success(m) => Some(m)
		case Failure(e) => DisplayUtils.none("MultiLinearRegression model undefined", logger, e)
	}
	
		/**
		 * <p>Retrieve the weight of the multi-variable linear regression
		 * if model has been successfully trained, None otherwise.</p>
		 * @return weights if model is successfully created, None otherwise
		 */
	final def weights: Option[DblVector] = model.map(_.weights)
	
		/**
		 * <p>Retrieve the residual sum of squares for this multi-variable linear regression
		 * if model has been successfully trained, None otherwise.</p>
		 * @return residual sum of squares if model is successfully created, None otherwise
		 */
	final def rss: Option[Double] = model.map(_.rss)

		/**
		 * <p>Test if the model has been trained and is defined.</p>
		 * @return true is the model has been trained, false otherwise
		 */
	final def isModel = model != None
	
		/**
		 * <p>Data transformation that predicts the value of a vector input.</p>
		 * @throws MatchError if the model is undefined or has an incorrect size
		 * @return PartialFunction of feature of type Array[T] as input and the predicted value of 
		 * type Double as output
		 */
	override def |> : PartialFunction[Feature, Double] = {
		case x: Feature if( !x.isEmpty && model != None && x.size == model.get.size-1) => {
				// Retrieve the regression weights (or coefs)
			val w = model.get.weights
					// Compute the dot product with w0 + w1.x1 + .. wn.xn
			x.zip(w.drop(1)).foldLeft(w(0))((s, z) => s + z._1*z._2)
		}
	}
	
	private def train: Try[RegressionModel] = {
		Try {
			// Force a conversion to DblMatrix 
			val xtv: DblMatrix = xt 
			
				// Create a sample for the label y
			newSampleData(y, xtv)
			
				// Invoke methods of the OLSMultipleLinearRegression class from commons math.
			val wRss = (estimateRegressionParameters, calculateResidualSumOfSquares)
			
				// Create a regression model with the weights and the resisual sum of squared errorrs
			RegressionModel(wRss._1, wRss._2)
		} 
	}
}



		/**
		 * <p>Companion object that defines the constructor for the class MultiLinearRegression 
		 * and validate its parameters</p>
		 * 
		 * @author Patrick Nicolas
		 * @since April 19, 2014
		 * @note Scala for Machine Learning Chapter 6 Regression and regularization / Ordinary least 
		 * squares regression
		 */
object MultiLinearRegression {
		/**
		 * Default constructor for the MultiLinearRegression class
		 * @param xt Input multi-dimensional time series for which regression is to be computed.
		 * @param y Labeled data for the Multivariate linear regression
		 */
	def apply[T <% Double](xt: XTSeries[Array[T]], y: DblVector): MultiLinearRegression[T] = 
			new MultiLinearRegression[T](xt, y)
			
			// Class parameters validation function
	private def check[T <% Double](xt: XTSeries[Array[T]], y: DblVector): Unit = {
		require( !xt.isEmpty,
				"MultiLinearRegression.check Cannot create a regression with undefined time series")
		require( !y.isEmpty, 
				"MultiLinearRegression.check  Cannot train aregression model with undefined labels")
		require (xt.size == y.size, 
			s"MultiLinearRegression.check Size of input${xt.size} != size of labels ${y.size}")
	}
}

// ------------------------------  EOF ---------------------------------------
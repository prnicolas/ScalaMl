/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99.1
 */
package org.scalaml.supervised.regression.linear

	// Scala classes
import scala.util.Try
import scala.language.implicitConversions
import scala.annotation.implicitNotFound

	// ScalaML classes
import org.scalaml.libraries.commonsmath.RidgeRAdapter

import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.ITransform
import org.scalaml.supervised.regression.{Regression, RegressionModel}
import org.scalaml.util.LoggingUtils
import ScalaMl._, Regression._


		/**
		 * Definition of the Ridge regression (linear least squares regression
		 * with a L2 penalty form). The training is executed during the instantiation
		 * of the class. The minimization of the loss function including the L2 regularization
		 * method uses a simple QR decomposition, although Cholesky factorization could be also used.'
		 * The implemantation follows the standard design of supervised learning algorithm:
		 * - The classifier implements the '''ITransform''' implicit monadic data transformation
		 * - The constructor triggers the training of the classifier, making the model immutable
		 * - The classifier implements the '''Monitor''' interface to collect profile information for 
		 * debugging purpose
		 * 
		 * {{{
		 * Ridge regression estimate  w' = argmin Sum [squares {y(i)  - f(x(i)|w)} + lambda.w.w]
		 *					with regression model f(x|w) = w(0) + w(1).x(1) + ... + w(n).x(n)
		 *							Residuals are defined as r(i) = Y(i) - X(i)*A
		 *							Residuals sum of squared error as rss = sqrt(SUM r(i)*r(i))
		 * }}}
		 * @tparam T type of features in observations
		 * @constructor Instantiates a Ridge regression model. 
		 * @throws IllegalArgumentException if the class parameters are undefined
		 * @param xt Time series of features observations
		 * @param expected Target/ labeled/ expected output values
		 * @param lambda L2 penalty factor.
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98.1 April 14, 2014
		 * @version 0.99.1
		 * @see org.apache.commons.math3.stat.regression.AbstractMultipleLinearRegression
		 * @see org.apache.commons.math3.linear._
		 * @see org.scalaml.core.ITransform
		 * @see org.scalaml.util.Monitor
		 * @see Scala for Machine Learning  Chapter 6 ''Regression and regularization'' / Ridge regression
		 */
@throws(classOf[IllegalArgumentException])
@implicitNotFound(msg = "RidgeRegression Implicit conversion $T to Double undefined")
final class RidgeRegression[T <: AnyVal](
		xt: XVSeries[T], 
		expected: DblVector, 
		lambda: Double)(implicit f: T => Double) extends ITransform[Array[T]](xt) with Regression {
	
	import RidgeRegression._
  type V = Double
	check(xt, expected)

		/**
		 * Data transformation that predicts the value of a vector input using the Ridge regression.
		 * 
		 * @throws MatchError if the model is undefined or has an incorrect size
		 * @return PartialFunction of feature of type Array[T] as input and the predicted value of 
		 * type Double as output
		 */
	override def |> : PartialFunction[Array[T], Try[V]] = {
		case x: Array[T] if isModel && x.length == model.get.size - 1
			=> Try ( dot(x, model.get) )
	}

	
	import scala.language.reflectiveCalls
	override protected def train:  RegressionModel = {
	  val mlr = new RidgeRAdapter(lambda, xt.head.length)
				// Invoke Apache Commons Math generation of the X and Y values.
		mlr.createModel(xt, expected)
				// Extract the tuple (regression weights, residual sum of squared errors)
		RegressionModel(mlr.getWeights, mlr.getRss) 
	} 
}


		/**
		 * Companion object for the Ridge regression. This singleton is used
		 * to validate the class parameters.
		 * 
		 * @author Patrick Nicolas
		 * @since April 14, 2014
		 * @note Scala for Machine Learning  Chapter 6 Regression and regularization / Ridge regression
		 */
object RidgeRegression {
	
		/**
		 * Default constructor for the Ridge regression
		 * @param xt Time series of features observations
		 * @param expected Target or labeled output values
		 * @param lambda L2 penalty factor.
		 */
	def apply[T <: AnyVal](
			xt: XVSeries[T], 
			expected: DblVector, 
			lambda: Double)(implicit f: T => Double): RidgeRegression[T] =
		new RidgeRegression(xt, expected, lambda)
	
	implicit def ridgeRegr2Try[T <: AnyVal](regr: RidgeRegression[T])(implicit f: T => Double): Try[RidgeRegression[T]] = 
		Try(regr)
	
	
	private def check[T <: AnyVal](xt: XVSeries[T], y: DblVector)(implicit f: T => Double): Unit = {
		require( xt.nonEmpty,
				"Cannot create Ridge regression model with undefined features")
		require( y.nonEmpty,
				"Cannot create Ridge regression model with undefined observed data")
		require(xt.size == y.length, 
				s"Size of the features set ${xt.size} differs for the size of observed data ${y.length}")
	}
}


// --------------------------  EOF -------------------------------
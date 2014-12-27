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
import scala.util.{Try, Success, Failure}
	// Third party libraries classes
import org.apache.log4j.Logger
import org.apache.commons.math3.stat.regression.AbstractMultipleLinearRegression
import org.apache.commons.math3.linear.{RealMatrix, RealVector, QRDecomposition, LUDecomposition}
import org.apache.commons.math3.stat.StatUtils
import org.apache.commons.math3.stat.descriptive.moment.SecondMoment
	// ScalaML classes
import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.XTSeries
import org.scalaml.core.Design.PipeOperator
import org.scalaml.supervised.regression.RegressionModel
import org.scalaml.util.DisplayUtils
import ScalaMl._


		/**
		 * <p>Definition of the Ridge regression (linear least squares regression
		 * with a L2 penalty form). The training is executed during the instantiation
		 * of the class. The minimization of the loss function including the L2 regularization
		 * method uses a simple QR decomposition, although Cholesky factorization could be also used.
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;
		 * ,Arial,Helvetica,sans-serif;">
		 * Ridge regression estimate w' = argmin Sum [squares {y(i)  - f(x(i)|w)} + lambda.w.w]<br>
		 * with regression model f(x|w) = w(0) + w(1).x(1) + ... + w(n).x(n)
		 * Residuals are defined as r(i) = Y(i) - X(i)*A
		 * Residuals sum of squared error as rss = sqrt(SUM r(i)*r(i))</span></pre></p>
		 * @constructor Instantiates a Ridge regression model. 
		 * @throws IllegalArgumentException if the class parameters are undefined
		 * @param xt Time series of features observations
		 * @param y Target or labeled output values
		 * @param lambda L2 penalty factor.
		 * @see org.apache.commons.math3.stat.regression.AbstractMultipleLinearRegression
		 * @see org.apache.commons.math3.linear._
		 * @see org.scalaml.core.Design.PipeOperator
		 * 
		 * @author Patrick Nicolas
		 * @since April 14, 2014
		 * @note Scala for Machine Learning  Chapter 6 Regression and regularization / Ridge regression
		 */
final class RidgeRegression[T <% Double](xt: XTSeries[Array[T]], y: DblVector, lambda: Double) 
					extends AbstractMultipleLinearRegression with PipeOperator[Array[T], Double] {
	
	import RidgeRegression._
	check(xt, y)
		/**
		 * Standard type for a featur
		 */
	type Feature = Array[T]
	
	private val logger = Logger.getLogger("RidgeRegression")
		// The linear optimization method (QR, Cholesky...) is selected at run time.
	private[this] var qr: QRDecomposition = _
	
		/**
		 * Model created during training/instantiation of the class. The model is
		 * instantiated only if the regression weights can be computed. 
		 */
	private[this] val model: Option[RegressionModel] = train match {
		case Success(m) => Some(m)
		case Failure(e) => DisplayUtils.none("RidgeRegression model undefined", logger, e)
	}	

		/**
		 * <p>Retrieve the weights of this Ridge regression model. The vector of the
		 * weights is returned if the model has been successfully created (trained).
		 * @return weight vector option if the model was successfully trained, None otherwise
		 */
	final def weights: Option[DblVector] = model.map(_.weights)

		/**
		 * <p>Retrieve the residuals sum of squares RSS of this Ridge regression model. The RSS
		 * value is returned if the model has been successfully created (trained).
		 * @return rss option if the model was successfully trained, None otherwise
		 */
	final def rss: Option[Double] = model.map( _.rss)

		/**
		 * <p>Test if the model has been trained and is defined.</p>
		 * @return true is the model has been trained, false otherwise
		 */
	final def isModel = model != None
	
		/**
		 * <p>Data transformation that predicts the value of a vector input using the Ridge regression.
		 * </p>
		 * @throws MatchError if the model is undefined or has an incorrect size
		 * @return PartialFunction of feature of type Array[T] as input and the predicted value of 
		 * type Double as output
		 */
	override def |> : PartialFunction[Feature, Double] = {
		case x: Feature if( !x.isEmpty && model != None && x.size == model.get.size-1) => {
				// Get the weights without intercept from the model
			val weights = model.get.weights.drop(1)
			
				// Apply the formula Y = w1.x1 + w2.x2 + ... + wn.xn + w0 
			x.zip(weights).foldLeft(weights(0))((s, z) => s + z._1*z._2)
		}
	}

		/**
		 * <p>Override the newXSampleData method of the Common Math class 
		 * <b>AbstractMultipleLinearRegression</b>.
		 * The purpose is to add a lambda components to the loss function</p>
		 * @param x Vector of features to be converted
		 */
	override protected def newXSampleData(x: DblMatrix): Unit =  {
		super.newXSampleData(x)
		
		val xtx: RealMatrix = getX	
			// Add the lambda (L2 regularization) component to the loss function
		Range(0, xt(0).size).foreach(i 
			=> xtx.setEntry(i, i, xtx.getEntry(i, i) + lambda) )
			
			// Uses a simple QR decomposition 
		qr = new QRDecomposition(xtx)
	}

		/**
		 * Override the computation of the beta value
		 * @return A vector with beta values of type RealVector
		 */
	override protected def calculateBeta: RealVector = qr.getSolver().solve(getY())

		/**
		 * <p>Override the calculateBetaVariance method of the Common Math class 
		 * <b>AbstractMultipleLinearRegression</b>.
		 * using the QR decomposition</p>
		 * @return the matrix of variance of model
		 */
	override protected def calculateBetaVariance: RealMatrix = {
		val colDim = getX().getColumnDimension
			
			// Extracts the matrix of residuals 
		val R = qr.getR().getSubMatrix(0, colDim - 1 , 0, colDim - 1)
		
			// Solve the linear system and compute the inverse matrix
			// using the LU decomposition.
		val Rinv = new LUDecomposition(R).getSolver.getInverse
		Rinv.multiply(Rinv.transpose);
	}
	
		
	private def train: Try[RegressionModel] = {
		Try {
				// Invoke Apache Commons Math generation of the X and Y values.
			this.newXSampleData(xt.toDblMatrix)
			newYSampleData(y)
			
				// Retrieve the residuals from AbstractMultipleLinearRegression class
				// then compute sum of squared errors using a map and sum.
			val _rss = calculateResiduals.toArray.map(x => x*x).sum
			
				// Extract the tuple (regression weights, residual sum of squared errors)
			val wRss = (calculateBeta.toArray, _rss)
			RegressionModel(wRss._1, wRss._2)
		} 
	}
   
		/**
		 * Compute the total sum of squared error. The computation uses
		 * the simple sum of squares value from Apache Commons Math, StatsUtils.sumSq
		 * in the case of No intercept values, or use the statistical second moment
		 */
	private def calculateTotalSumOfSquares: Double = 
		if (isNoIntercept) 
			StatUtils.sumSq(getY.toArray) 
		else 
			(new SecondMoment).evaluate(getY.toArray)
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
		 * @param y Target or labeled output values
		 * @param lambda L2 penalty factor.
		 */
	def apply[T <% Double](xt: XTSeries[Array[T]], y: DblVector, lambda: Double): RidgeRegression[T] =
		new RidgeRegression(xt, y, lambda)
	
	private def check[T <% Double](xt: XTSeries[Array[T]], y: DblVector): Unit = {
		require( !xt.isEmpty, 
				"Cannot create Ridge regression model with undefined features")
		require( !y.isEmpty, 
				"Cannot create Ridge regression model with undefined observed data")
		require(xt.size == y.size, 
				s"Size of the features set ${xt.size} differs for the size of observed data ${y.size}")
	}
}


// --------------------------  EOF -------------------------------
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
package org.scalaml.supervised.regression.linear

import org.scalaml.core.XTSeries
import org.scalaml.core.types.ScalaMl._
import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression
import org.apache.commons.math3.exception.{MathIllegalArgumentException, MathRuntimeException, OutOfRangeException}
import org.scalaml.core.design.PipeOperator
import org.scalaml.core.types.CommonMath._
import scala.annotation.implicitNotFound
import org.scalaml.supervised.regression.RegressionModel
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display
import scala.language.implicitConversions

		/**
		 * <p>Class that defines a Multivariate linear regression. The computation of the regression coefficients uses the 
		 * Apache commons Math library. The regression model (regression parameters or weights) are
		 * initialized only if the training was successful.</p>
		 * @constructor Creates multi-variate linear regression. [xt] Input multi-dimensional time series for which regression is to be computed. [y] labeled data for the Multivariate linear regression
		 * @param xt input multi-dimensional time series for which regression is to be computed
		 * @param y labeled data for the Multivariate linear regression
		 * @throws IllegalArgumentException if the input time series or the labeled data are undefined or have different sizes
		 * 
		 * @author Patrick Nicolas
		 * @since April 19, 2014
		 * @note Scala for Machine Learning Chapter 6 Regression and regularization $Ordinary least squares regression
		 */
@implicitNotFound("Implicit conversion to Double for MultiLinearRegression is missing")
final class MultiLinearRegression[T <% Double](xt: XTSeries[Array[T]], y: DblVector) 
                    extends OLSMultipleLinearRegression with PipeOperator[Array[T], Double] {
	
	require(xt != null && xt.size > 0, "Cannot create perform a Multivariate linear regression on undefined time series")
	require(y != null && y.size > 0, "Cannot train a Multivariate linear regression with undefined labels")
    require (xt.size == y.size, s"Size of Input data ${xt.size} and labels ${y.size} for Multivariate linear regression are difference")
		
    type Feature = Array[T]
	private val logger = Logger.getLogger("MultiLinearRegression")
	
	private[this] val model: Option[RegressionModel] = {
	  Try {
		 newSampleData(y, xt.toDblMatrix)
		 val wRss = (estimateRegressionParameters, calculateResidualSumOfSquares)
	 	 RegressionModel(wRss._1, wRss._2)
	  } match {
	  	case Success(m) => Some(m)
	  	case Failure(e) => Display.none("MultiLinearRegression.model ", logger, e)
	  }
	}
	
		/**
		 * <p>Retrieve the weight of the multi-variable linear regression
		 * if model has been successfully trained, None otherwise.</p>
		 * @return weights if model is successfully created, None otherwise
		 */
	final def weights: Option[DblVector] = model match {
		case Some(m) => Some(m.weights)
		case None => Display.none("MultiLinearRegression.weights: Model undefined", logger)
	}
	
		/**
		 * <p>Retrieve the residual sum of squares for this multi-variable linear regression
		 * if model has been successfully trained, None otherwise.</p>
		 * @return residual sum of squares if model is successfully created, None otherwise
		 */
	final def rss: Option[Double] = model match {
		case Some(m) => Some(m.rss)
		case None => Display.none("MultiLinearRegression.rss: Model undefined", logger)
	}

    
		/**
		 * <p>Data transformation that predicts the value of a vector input.</p>
		 * @throws MatchError if the model is undefined or has an incorrect size
   		 * @return PartialFunction of feature of type Array[T] as input and the predicted value of type Double as output
		 */
	override def |> : PartialFunction[Feature, Double] = {
	   case x: Feature if(x != null && model != None && x.size != model.get.size-1) =>  {
		  val w = model.get.weights
          x.zip(w.drop(1)).foldLeft(w(0))((s, z) => s + z._1*z._2)
       }
	}
}



		/**
		 * <p>Companion object that defines the 
		 * constructor for the class MultiLinearRegression.</p>
		 * 
		 * @author Patrick Nicolas
		 * @since April 19, 2014
		 */
object MultiLinearRegression {
	def apply[T <% Double](xt: XTSeries[Array[T]], y: DblVector): MultiLinearRegression[T] = new MultiLinearRegression[T](xt, y)
}

// ------------------------------  EOF ---------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95c
 */
package org.scalaml.supervised.regression.linear

import org.apache.commons.math3.stat.regression.AbstractMultipleLinearRegression
import org.apache.commons.math3.linear.{RealMatrix, RealVector, QRDecomposition, LUDecomposition}
import org.scalaml.core.types.ScalaMl._
import org.scalaml.core.XTSeries
import scala.annotation.implicitNotFound
import org.apache.commons.math3.stat.StatUtils
import org.apache.commons.math3.stat.descriptive.moment.SecondMoment
import org.scalaml.core.design.PipeOperator
import org.scalaml.supervised.regression.RegressionModel
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display



		/**
		 * <p>Definition of the Ridge regression (linear least squares regression
		 * with a L2 penalty form). The training is executed during the instantiation
		 * of the class.</p>
		 * @constructor Instantiates a Ridge regression model. [xt] Time series of features observations. [y] Target or labeled output values. [lambda] L2 penalty factor
		 * @param xt Time series of features observations
		 * @param y target or labeled output values
		 * @param lambda L2 penalty factor
		 * @throws IllegalArgumentException if the class parameters are undefined
		 * @see org.apache.commons.math3.stat.regression
		 * 
		 * @author Patrick Nicolas
		 * @since April 14, 2014
		 * @note Scala for Machine Learning  Chapter 6 Regression and regularization/Ridge regression
		 */
final class RidgeRegression[T <% Double](val xt: XTSeries[Array[T]], val y: DblVector, val lambda: Double) 
                  extends AbstractMultipleLinearRegression with PipeOperator[Array[T], Double] {
	
	check(xt, y)

    type Feature = Array[T]
	
	private val logger = Logger.getLogger("RidgeRegression")
	private var qr: QRDecomposition = _
	
		// Model created during training/instantiation of the class
  	private[this] val model: Option[RegressionModel] = {	
	  Try {
		 this.newXSampleData(xt.toDblMatrix)
		 newYSampleData(y)
		 val _rss = calculateResiduals.toArray.map(x => x*x).sum
	 	 val wRss = (calculateBeta.toArray, _rss)
	 	 RegressionModel(wRss._1, wRss._2)
	  } match {
	  	case Success(m) => Some(m)
	  	case Failure(e) => Display.none("RidgeRegression.model could not be trained", logger, e)
	  }
	}
    
    	/**
    	 * <p>Retrieve the weights of this Ridge regression model. The vector of the
    	 * weights is returned if the model has been successfully created (trained).
    	 * @return weight vector option if the model was successfully trained, None otherwise
    	 */
    final def weights: Option[DblVector] = model match {
       case Some(m) => Some(m.weights)
       case None =>  Display.none("RidgeRegression.weights model undefined", logger)
    }
    
        /**
    	 * <p>Retrieve the residuals sum of squares RSS of this Ridge regression model. The RSS
    	 * value is returned if the model has been successfully created (trained).
    	 * @return rss option if the model was successfully trained, None otherwise
    	 */
    final def rss: Option[Double] = model match {
    	case Some(m) => Some(m.rss)
    	case None => Display.none("RidgeRegression.rss model undefined", logger)
    }
    
    	/**
		 * <p>Data transformation that predicts the value of a vector input using the Ridge regression.</p>
		 * @throws MatchError if the model is undefined or has an incorrect size
   		 * @return PartialFunction of feature of type Array[T] as input and the predicted value of type Double as output
		 */
   override def |> : PartialFunction[Feature, Double] = {
     case x: Feature if(x != null && model != None && x.size != model.get.size+1) => {
        val m = model.get
        x.zip(m.weights.drop(1)).foldLeft(m.weights(0))((s, z) => s + z._1*z._2)
     }
   }

   		/**
   		 * <p>Override the newXSampleData
   		 */
   override protected def newXSampleData(x: DblMatrix): Unit =  {
        super.newXSampleData(x)
        val xtx: RealMatrix = getX
        Range(0, xt(0).size).foreach( i => xtx.setEntry(i, i, xtx.getEntry(i, i) + lambda) )
        qr = new QRDecomposition(xtx)
     }
  
   override protected def calculateBeta: RealVector = qr.getSolver().solve(getY())

    
   override protected def calculateBetaVariance: RealMatrix = {
  	   val colDim = getX().getColumnDimension
       val R = qr.getR().getSubMatrix(0, colDim - 1 , 0, colDim - 1)
       val Rinv = new LUDecomposition(R).getSolver.getInverse
       Rinv.multiply(Rinv.transpose);
   }
   
   private def calculateTotalSumOfSquares: Double = 
       if (isNoIntercept)  StatUtils.sumSq(getY.toArray) else (new SecondMoment).evaluate(getY.toArray)
       
   private def check(xt: XTSeries[Array[T]], y: DblVector): Unit = {
	    require(xt != null && xt.size > 0, "Cannot create Ridge regression model with undefined features")
		require(y != null && y.size > 0, "Cannot create Ridge regression model with undefined observed data")
		require(xt.size == y.size, "Size of the features set " + xt.size + " differs for the size of observed data " + y.size)
	}
}


// --------------------------  EOF -------------------------------
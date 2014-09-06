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

import org.apache.commons.math3.stat.regression.AbstractMultipleLinearRegression
import org.apache.commons.math3.linear.{RealMatrix, RealVector, QRDecomposition, LUDecomposition}
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.XTSeries
import scala.annotation.implicitNotFound
import org.apache.commons.math3.stat.StatUtils
import org.apache.commons.math3.stat.descriptive.moment.SecondMoment
import org.scalaml.workflow.PipeOperator
import org.scalaml.supervised.regression.RegressionModel
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display



		/**
		 * <p>Definition of the Ridge regression (linear least squares regression
		 * with a L2 penalty form). The training is executed during the instantiation
		 * of the class.</p>
		 * @param xt Time series of features observations
		 * @param y target or labeled output values
		 * @param lambda L2 penalty factor
		 * @throws IllegalArgumentException if the class parameters are undefined
		 * 
		 * @author Patrick Nicolas
		 * @since April 14, 2014
		 * @note Scala for Machine Learning
		 */
final class RidgeRegression[T <% Double](val xt: XTSeries[Array[T]], val y: DblVector, val lambda: Double) 
                  extends AbstractMultipleLinearRegression with PipeOperator[Array[T], Double] {
	
	validate(xt, y)

    type Feature = Array[T]
	
	private val logger = Logger.getLogger("RidgeRegression")
	private var qr: QRDecomposition = null
  	private[this] val model: Option[RegressionModel] = {
    	
	  Try {
		 this.newXSampleData(xt.toDblMatrix)
		 newYSampleData(y)
		 val _rss = calculateResiduals.toArray.map(x => x*x).sum
	 	 val wRss = (calculateBeta.toArray, _rss)
	 	 RegressionModel(wRss._1, wRss._2)
	  } match {
	  	case Success(m) => Some(m)
	  	case Failure(e) => Display.error("RidgeRegression.model ", logger, e); None
	  }
	}
    
    	/**
    	 * <p>Retrieve the weights of this Ridge regression model. The vector of the
    	 * weights is returned if the model has been successfully created (trained).
    	 * @return weight vector option if the model was successfully trained, None otherwise
    	 */
    final def weights: Option[DblVector] = model match {
       case Some(m) => Some(m.weights)
       case None => None
    }
    
        /**
    	 * <p>Retrieve the residuals sum of squares RSS of this Ridge regression model. The RSS
    	 * value is returned if the model has been successfully created (trained).
    	 * @return rss option if the model was successfully trained, None otherwise
    	 */
    final def rss: Option[Double] = model match {
    	case Some(m) => Some(m.rss)
    	case None => None
    }
    
    	/**
		 * <p>Data transformation that predicts the value of a vector input using the Ridge regression.</p>
		 * @param x Array of parameterized values
		 * @throws IllegalStateException if the size of the input vector is incorrect
		 * @return predicted value if the model has been successfully trained, None otherwise
		 */
    override def |> (x: Feature): Option[Double] = model match {
      case Some(m) => {
    	 if( x == null || x.size != m.size +1) 
    		 throw new IllegalStateException("Size of input data for prediction " + x.size + " should be " + (m.size -1))
         Some(x.zip(m.weights.drop(1)).foldLeft(m.weights(0))((s, z) => s + z._1*z._2))
      }
      case None => None
    }
  
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
       
   private def validate(xt: XTSeries[Array[T]], y: DblVector): Unit = {
	    require(xt != null && xt.size > 0, "Cannot create Ridge regression model with undefined features")
		require(y != null && y.size > 0, "Cannot create Ridge regression model with undefined observed data")
		require(xt.size == y.size, "Size of the features set " + xt.size + " differs for the size of observed data " + y.size)
	}
}


// --------------------------  EOF -------------------------------
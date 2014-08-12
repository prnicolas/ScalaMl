/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.supervised.regression.linear

import org.apache.commons.math3.stat.regression.AbstractMultipleLinearRegression
import org.apache.commons.math3.linear.{RealMatrix, RealVector, QRDecomposition, LUDecomposition}
import org.apache.commons.math3.exception.{MathIllegalArgumentException, MathRuntimeException, OutOfRangeException}
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.XTSeries
import scala.annotation.implicitNotFound
import org.apache.commons.math3.stat.StatUtils
import org.apache.commons.math3.stat.descriptive.moment.SecondMoment
import org.scalaml.workflow.PipeOperator
import org.scalaml.supervised.regression.RegressionModel



@implicitNotFound("Implicit conversion of type to Double for SingleLinearRegression is missing")
final class RidgeRegression[T <% Double](val xt: XTSeries[Array[T]], val labels: DblVector, val lambda: Double) 
                  extends AbstractMultipleLinearRegression with PipeOperator[Array[T], Double] {
	
    require(xt != null && xt.size > 0, "Cannot create Ridge regression model with undefined features")
	require(labels != null && labels.size > 0, "Cannot create Ridge regression model with undefined observed data")
	require(xt.size == labels.size, "Size of the features set " + xt.size + " differs for the size of observed data " + labels.size)
	  
	type Feature = Array[T]
	
	private var qr: QRDecomposition = null
  
  	private val model: Option[RegressionModel] = {
    	
	  try {
		this.newXSampleData(xt.toDblMatrix)
		newYSampleData(labels)
	 	val weightsRss = (calculateBeta.toArray, calculateTotalSumOfSquares)
	 	Some(RegressionModel(weightsRss._1, weightsRss._2))
	  }
	  catch {
		 case e: MathIllegalArgumentException => println("Cannot compute regression coefficients: " + e.toString); None
		 case e: MathRuntimeException => println("Cannot compute regression coefficients: " + e.toString); None
		 case e: OutOfRangeException => println("Cannot compute regression coefficients: " + e.toString); None
	  }
	}
    
    	
    def weights: Option[DblVector] = model match {
       case Some(m) => Some(m.weights)
       case None => None
    }
    
    def rss: Option[Double] = model match {
    	case Some(m) => Some(m.accuracy)
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
}


// --------------------------  EOF -------------------------------
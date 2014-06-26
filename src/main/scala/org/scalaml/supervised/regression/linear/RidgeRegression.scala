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



@implicitNotFound("Implicit conversion of type to Double for SingleLinearRegression is missing")
final class RidgeRegression[T <% Double](val xt: XTSeries[Array[T]], val y: XTSeries[T], val lambda: Double) extends AbstractMultipleLinearRegression {
    require(xt != null && xt.size > 0, "Cannot create Ridge regression model with undefined features")
	require(y != null && y.size > 0, "Cannot create Ridge regression model with undefined observed data")
	require(xt.size == y.size, "Size of the features set " + xt.size + " differs for the size of observed data " + y.size)
	  
	private var qr: QRDecomposition = null
  
  	private val weightsRss: Option[(DblVector, Double)] = {
	  try {
		this.newXSampleData(xt.toDblMatrix)
		newYSampleData(y.toDblVector)
	 	Some((calculateBeta.toArray, calculateTotalSumOfSquares))
	  }
	  catch {
		 case e: MathIllegalArgumentException => println("Cannot compute regression coefficients: " + e.toString); None
		 case e: MathRuntimeException => println("Cannot compute regression coefficients: " + e.toString); None
		 case e: OutOfRangeException => println("Cannot compute regression coefficients: " + e.toString); None
	  }
	}
    
    def weights: Option[DblVector] = {
    	weightsRss match {
    		case Some(wr) => Some(wr._1)
    		case None => None
    	}
    }
    
    def rss: Option[Double] = {
    	weightsRss match {
    		case Some(wr) => Some(wr._2)
    		case None => None
    	}
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
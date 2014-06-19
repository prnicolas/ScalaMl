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


@implicitNotFound("Implicit conversion of type to Double for SingleLinearRegression is missing")
final class RidgeRegression[@specialized(Double) T <% Double](val xt: XTSeries[Array[T]], val y: XTSeries[T], val lambda: Double) extends AbstractMultipleLinearRegression {
    private var qr: QRDecomposition = null
  
  	val weights: Option[DblVector] = {
	  try {
		this.newXSampleData(xt.toDblMatrix)
		newYSampleData(y.toDblVector)
	 	Some(calculateBeta.toArray)
	  }
	  catch {
		 case e: MathIllegalArgumentException => println("Cannot compute regression coefficients: " + e.toString); None
		 case e: MathRuntimeException => println("Cannot compute regression coefficients: " + e.toString); None
		 case e: OutOfRangeException => println("Cannot compute regression coefficients: " + e.toString); None
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
}


// --------------------------  EOF -------------------------------
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
package org.scalaml.libraries.commonsmath

import scala.language.implicitConversions

import org.apache.commons.math3.stat.regression.AbstractMultipleLinearRegression
import org.apache.commons.math3.linear.{RealMatrix, RealVector, QRDecomposition, LUDecomposition}
import org.apache.commons.math3.stat.StatUtils
import org.apache.commons.math3.stat.descriptive.moment.SecondMoment

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.libraries.commonsmath.CommonsMath._

/**
		 * Implicit conversion from internal primitive types DblArray and DblMatrix to Apache 
		 * Commons Math types.
		 * @author Patrick Nicolas
		 * @since January 23, 2014 0.98
		 * @version 0.98.1
		 * @see Scala for Machine Learning Chapter 3 Data pre-processing/Time series
		 */

	class RidgeRAdapter(lambda: Double, dim: Int) extends AbstractMultipleLinearRegression {
	
				// The linear optimization method (QR, Cholesky...) is selected at run time.
		private[this] var qr: QRDecomposition = _
			
		def createModel(x: DblMatrix, y: DblVector): Unit = {
			this.newXSampleData(x)
			super.newYSampleData(y.toArray)
		}
			
		final def getWeights: DblArray = calculateBeta.toArray
		final def getRss: Double = rss
			
		  /**
			 * Override the newXSampleData method of the Common Math class 
			 * '''AbstractMultipleLinearRegression'''.
			 * The purpose is to add a lambda components to the loss function
			 * @param x Vector of features to be converted
			 */
		override protected def newXSampleData(x: DblMatrix): Unit =  {
			super.newXSampleData(x)
			
			val xtx: RealMatrix = getX	
				
				// Add the lambda (L2 regularization) component to the loss function
			Range(0, dim).foreach(i 
						=> xtx.setEntry(i, i, xtx.getEntry(i,i) + lambda) )
				
				// Uses a simple QR decomposition 
			qr = new QRDecomposition(xtx)
		}
		
		
		def getBetaVariance: DblMatrix = calculateBetaVariance.getData
		  
			/**
			 * Override the computation of the beta value
			 * @return A vector with beta values of type RealVector
			 */
		override protected def calculateBeta: RealVector = qr.getSolver.solve(getY)
			
			 /**
			 * Override the calculateBetaVariance method of the Common Math class 
			 * '''AbstractMultipleLinearRegression'''.
			 * using the QR decomposition
			 * @return the matrix of variance of model
			 */
		override protected def calculateBetaVariance: RealMatrix = {
			val colDim = getX.getColumnDimension
					
					// Extracts the matrix of residuals 
			val R = qr.getR.getSubMatrix(0, colDim - 1 , 0, colDim - 1)
				
					// Solve the linear system and compute the inverse matrix
					// using the LU decomposition.
			val Rinv = new LUDecomposition(R).getSolver.getInverse
			Rinv.multiply(Rinv.transpose)
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
					
					// Retrieve the residual values from AbstractMultipleLinearRegression class
					// then compute sum of squared errors using a map and sum.
		private def rss: Double = {
			val x: Array[Double] = calculateResiduals
			x.aggregate(0.0)((s, _x) => s + sqr(_x), _ + _)
		}
	}


// ---------------------------  EOF -----------------------------
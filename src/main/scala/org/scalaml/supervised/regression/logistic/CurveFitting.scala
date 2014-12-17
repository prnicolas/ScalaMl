/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.2
 */
package org.scalaml.supervised.regression.logistic


import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.Matrix
import scala.util.Random
import org.apache.commons.math3.fitting.leastsquares.GaussNewtonOptimizer
import org.apache.commons.math3.optim.{SimpleVectorValueChecker, PointVectorValuePair}
import org.apache.commons.math3.linear.{RealVector, RealMatrix, MatrixUtils,Array2DRowRealMatrix, ArrayRealVector}
import org.apache.commons.math3.fitting.leastsquares.{MultivariateJacobianFunction, LeastSquaresBuilder, LeastSquaresProblem}
import org.apache.commons.math3.util.Pair
import org.apache.commons.math3.optim.ConvergenceChecker
import org.apache.commons.math3.optim.PointValuePair
import scala.language.implicitConversions
   
/**
 *  @author Patrick Nicolas
 *  @since Jun 21, 2014
 *  @note Book
 */
class CurveFitting[T <% Double](val x: DblVector, val y: Array[Double], val maxIters: Int, val maxEval: Int, val lambda: Double ) {
	
	
	val weights = Array.fill(3)( 0.5 )
	
	def train: Unit = {
		class MyFunction extends MultivariateJacobianFunction {
		   override def value(w: RealVector): Pair[RealVector, RealMatrix] = {
		  	  require( w.length> 0, "Cannot compute value of undefined data")

		  	  val nW = w.toArray
		  	  val f = new ArrayRealVector(x.map(t =>  nW(0) + nW(1)*t + nW(2)*t*t))
		      var jac = Array.ofDim[Double](x.length, weights.size)
		      x.zipWithIndex.foreach( t => { jac(t._2)(2) = t._1*t._1; jac(t._2)(1) = t._1; jac(t._2)(0) = 1.0} )

		      new Pair[RealVector, RealMatrix] (f, new Array2DRowRealMatrix(jac))
			}
		}
	    

	    val convergenceChecker = new ConvergenceChecker[PointVectorValuePair] {
	    	override def converged(iteration: Int, prev: PointVectorValuePair, current: PointVectorValuePair): Boolean =  {
	    		println("Convergence iterations " + iteration + "\nPrev")
	    		
	    		val delta = prev.getValue.zip(current.getValue).foldLeft(0.0)( (s, z) => { val diff = z._1 - z._2;  s + diff*diff} )
	    		println("delta: " + delta)
	    		iteration > maxIters-20
	    	}
	    }
	    val builder = new LeastSquaresBuilder
        val lsp = builder.model(new MyFunction)
                          .weight(MatrixUtils.createRealDiagonalMatrix(Array.fill(x.size)(1.0))) 
                          .target(y)
                         .checkerPair(convergenceChecker)
                         .maxEvaluations(maxEval)
                         .start(weights)
                         .maxIterations(maxIters)
                         .build
        
	    val gaussNewton = new GaussNewtonOptimizer
        val optimum = gaussNewton.optimize(lsp)
        println("Optimum: ")
        optimum.getPoint.toArray.zipWithIndex.foreach( t =>  println(t._2 + " => " + t._1) )
	}
}


object CurveFitting {
	implicit def pairToTuple[U, V](pair: Pair[U, V]): (U,V) = (pair._1, pair._2)
}

/*
object CurveFittingApp extends App {
	val xV: DblVector = Range(1, 20).map(_.toDouble).toArray
	val yV: DblVector = xV.map( x=> 1.0 - 2.0*x + x*x+ 0.1*Random.nextDouble )
	
	val logIt = new CurveFitting[Double](xV ,yV, 300, 500, 0.5)
	logIt.train
}
*/
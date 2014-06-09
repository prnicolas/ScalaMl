/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.filtering


import org.apache.commons.math3.linear._
import org.apache.commons.math3.filter._
import org.scalaml.core.XTSeries
import scala.util.Random
import org.scalaml.core.Types
import scala.Array.canBuildFrom
import org.scalaml.workflow.PipeOperator
import org.apache.commons.math3.exception.DimensionMismatchException
import scala.annotation.implicitNotFound


	/**
	 * <p>Case class that defines the white (Gaussian) noise q for the process 
	 * and the white noise r for the measurement or observation devices. An instance
	 * of this class has to be provided implicitly in the scope of the Kalman filter
	 * instance (DKalman).</p>
	 * @param qr  tuples that define the mean values of the process and measurement noise
	 * @paran white scalar function that specify the white noise distribution
	 * @exception IllegalArgumentException if the white noise generator is undefined
	 * @author Patrick Nicolas
	 * @date February 12, 2014
	 * @project Scala for Machine Learning
	 */
import Types.ScalaMl._
case class QRNoise(val qr: XY, val white: Double=> Double) {
  require( white != null, "Cannot define the noise for the Kalman filter with undefined white noise function")
  
  def q = white(qr._1) 
  def r = white(qr._2) 
  def create(m: XY): DblVector = Array[Double](white(m._1), white(m._2))
  def create(m: Double): DblVector = Array[Double](white(m))
}


		/**
		 * <p>Kalman filter class that uses a recursive optimal filter. The client code has to provide the
		 * time independent state transition t to t+1, the input matrix B. the measurement dependency matrix, H
		 * and the error matrix P. This implementation uses the Apache Commons math library. The process and
		 * measurement white noise is provided as an implicit value.</p>
		 * @param A  State transition matrix
		 * @param B input state matrix
		 * @param H matrix that defines the dependency of the measurement on the state of the system
		 * @param P error matrix
		 * @param qrNoise implicit value representing the white noise for the process Q and the measurement P
		 * @exception IllegalArgumentException if the input matrices are undefined or have inconsistent dimension
		 * @author Patrick Nicolas
		 * @date February 11, 2014
		 * @project Scala for Machine Learning
		 */
@implicitNotFound("White noise, QRNoise, has to be implicitly defined for Kalman filter")
class DKalman(val A: DblMatrix,  val B: DblMatrix,  val H: DblMatrix, val P: DblMatrix)(implicit val qrNoise: QRNoise) 
   extends PipeOperator[XTSeries[XY], XTSeries[XY]] {
	
  import Types.ScalaMl._, DKalman._
  
  require(A != null && B != null && H != null && P != null, "Cannot create a Kalman filter with undefined parameters")
  require( A.size == B.size && A(0).size == H(0).size, 
  		  "Incorrect dimension in Kalman filter A(" + A.size + "x" + A(0).size + ") and H(" + H.size + "x" + H(0).size + ")")
  
  
  val Q =  new DblMatrix(A.size).map( x => Array.fill(A.size)(qrNoise.q) )
  val R = Array.fill(1)(qrNoise.r)

  private var filter: KalmanFilter = null;
  private var x: RealVector = null
  
		 /**
		  * <p>Overloaded pipe operator to implement the Kalman filter as a data transformation. The
		  * exception thrown by the Apache Commons Math library are internally caught in case of 
		  * computational failure and the data transformation return None.</p>
		  * @param xt time series of tuple of type XY =(Double, Double) for {x,y} values
		  * @return the time series {x,y} filtered by the Kalman algorithm if no exception throw, None otherwise.
		  * @exception IllegalArgumentException if the input time series is undefined.
		  */
  override def |> (xt: XTSeries[XY]): Option[XTSeries[XY]] = {
  	require(xt != null && xt.size > 0, "Cannot apply Kalman filter to undefined time series")
  	
  	try {
	     val filtered = xt.map( y => { 
	        initialize(Array[Double](y._1, y._2))
	        val nState = newState
	        (nState(0), nState(1))
	     })  
	     Some(filtered)
  	}
  	catch {
  		case e: NonSquareMatrixException => Console.println(e.toString); None
  		case e: DimensionMismatchException =>  Console.println(e.toString); None
  		case e: MatrixDimensionMismatchException =>  Console.println(e.toString); None
  		case e: RuntimeException =>  Console.println(e.toString); None
  	 }
   }
  
		  	/**
		  	 * Initialize the Kalman filter 
		  	 */
  private def initialize(input: DblVector): Unit = {
  	 require(input != null && input.size >0, "Cannot initialize aKalman filter with incorrect data")
  	
     val pModel = new DefaultProcessModel(A, B, Q, input, P)
     val mModel = new DefaultMeasurementModel(H, R)                 
     filter = new KalmanFilter(pModel, mModel)
     x = new ArrayRealVector(input)
  }
  
  private def newState: DblVector = {
      import Types.ScalaMl._
      import Types.CommonMath._
      
      var k = 0
      while( k < 2) {  
 	     filter.predict

	     try {
		     x = A.operate(x).add(qrNoise.create(0.03, 0.1)) 
		     val z = H.operate(x).add(qrNoise.create(0.4))
		     filter.correct(z)
	     }
	     catch { 
	    	case e: NonPositiveDefiniteMatrixException => k = 6
	    }
		 k += 1
	  }
      val y = filter.getStateEstimation
      println ("y = " + y(0) + "," + y(1))
      y
  }

}

		/**
		 * Companion object for the Kalman filter DKalman that defines its constructor.
		 */
object DKalman {
   def apply(A: DblMatrix, B: DblMatrix, H: DblMatrix, P: DblMatrix)(implicit qrNoise: QRNoise): DKalman = new DKalman(A, B, H,P)(qrNoise)
}



// -----------------------  EOF ---------------------------------
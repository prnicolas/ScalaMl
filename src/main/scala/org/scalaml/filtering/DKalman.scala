/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.94
 */
package org.scalaml.filtering

import scala.util.Random
import scala.Array.canBuildFrom
import scala.annotation.implicitNotFound
import org.apache.commons.math3.linear._
import org.apache.commons.math3.filter._
import org.scalaml.core.XTSeries
import org.scalaml.core.types
import org.scalaml.core.design.PipeOperator
import types.ScalaMl._
import scala.util.{Try, Success, Failure}
import org.scalaml.util.Display
import org.apache.log4j.Logger

	/**
	 * <p>Case class that defines the white (Gaussian) noise q for the process 
	 * and the white noise r for the measurement or observation devices. An instance
	 * of this class has to be provided implicitly in the scope of the Kalman filter
	 * instance (DKalman).</p>
	 * @constructor Create an instance of QRNoise with a tuple {mean q noise, mean R noise}. [qr]: Tuple of mean of process (Q) noise and mean of measurement (R) noise [white]: Profile of the white noise
	 * @param qr  tuples that define the mean values of the process and measurement noise
	 * @param white scalar function that specify the white noise distribution
	 * @throws IllegalArgumentException if the white noise generator is undefined
	 * @author Patrick Nicolas
	 * @since February 12, 2014
	 * @note Scala for Machine Learning
	 */

case class QRNoise(qr: XY, white: Double=> Double) {
  require( white != null, "Cannot define the noise for the Kalman filter with undefined white noise function")
  
  	/**
  	 * <p>Compute the white noise for process Q</p>
  	 * @return white noise (stochastic) value for process Q
  	 */
  private def q = white(qr._1) 
  
    /**
  	 * <p>Compute the white noise for measurement R</p>
  	 * @return white noise (stochastic) value for measurement R
  	 */
  private def r = white(qr._2) 
  
  	/**
  	 * <p>Create an array of tuple of white noise values for process and white noise values for measurement.</p>
  	 * @param m tuple of mean values of white noise for process and white noise for measurement
  	 * @return Array of tuple of white noise (Q and R) values
  	 */
  def noisyQ: DblVector = Array[Double](q, q)
  
  def noisyR: DblVector = Array[Double](r, r)
}


		/**
		 * <p>Kalman filter class that uses a recursive optimal filter. The client code has to provide the
		 * time independent state transition t to t+1, the input matrix B. the measurement dependency matrix, H
		 * and the error matrix P. This implementation uses the Apache Commons math library. The process and
		 * measurement white noise is provided as an implicit value.</p>
		 * @constructor Create a scalar Kalman filter: [A] State transition matrix, [B] Control state matrix, [H] Matrix that defines the dependency of the measurement on the state of the system, [P] Covariance error matrix,  [qrNoise] Implicit value representing the white noise for the process Q and the measurement P
		 * @throws IllegalArgumentException if the input matrices are undefined or have inconsistent dimension
		 * @author Patrick Nicolas
		 * @since February 11, 2014
		 * @note Scala for Machine Learning
		 */
@implicitNotFound("White noise, QRNoise, has to be implicitly defined for Kalman filter")
class DKalman(A: DblMatrix,  B: DblMatrix,  H: DblMatrix, P: DblMatrix)(implicit val qrNoise: QRNoise) 
   extends PipeOperator[XTSeries[XY], XTSeries[XY]] {
	
  import types.ScalaMl._, DKalman._
   
  validateDimension(A, B,  H, P)
  private val logger = Logger.getLogger("DKalman")
  		/**
  		 * <p>Process related white noise (mean = 0.0)
  		 */
  private[this] val Q = new DblMatrix(A.size).map(_ => Array.fill(A(0).size)(qrNoise.qr._1) )
      	/**
  		 * <p>Measurement related white noise (mean = 0.0)
  		 */
  private[this] val R = new DblMatrix(A.size).map(_ => Array.fill(A(0).size)(qrNoise.qr._2) )

  private var filter: KalmanFilter = _
  private var x: RealVector = _
  
		 /**
		  * <p>Overloaded pipe operator to implement the Kalman filter as a data transformation. The
		  * exception thrown by the Apache Commons Math library are internally caught in case of 
		  * computational failure and the data transformation return None.</p>
		  * @throws MatchError if the input time series is undefined or have no elements
   		  * @return PartialFunction of time series of elements of type T as input to the Discrete Fourier filter and time series of frequencies of type Double as output
		  * @param xt time series of tuple of type XY =(Double, Double) for {x,y} values
		  */
  override def |> : PartialFunction[XTSeries[XY], XTSeries[XY]] = {
  	 case xt: XTSeries[XY] if(xt != null && xt.length > 0) => xt.map( y => {
  		 initialize(Array[Double](y._1, y._2))
	     val nState = newState
	     (nState(0), nState(1))
     })
  }

  
		  	/**
		  	 * Initialize the Kalman filter class of Apache Commons Math with an input of values
		  	 */
  private def initialize(input: DblVector): Unit = {  	
     val pModel = new DefaultProcessModel(A, B, Q, input, P)
     val mModel = new DefaultMeasurementModel(H, R)                 
     filter = new KalmanFilter(pModel, mModel)
     x = new ArrayRealVector(input)
  }
  
  private def newState: DblVector = {
      import types.CommonMath._

      filter.predict
      x = A.operate(x).add(qrNoise.noisyQ) 
      val z = H.operate(x).add(qrNoise.noisyR)
      filter.correct(z)
      filter.getStateEstimation
  }
  
  
  private def lsError(x: RealVector, z: RealVector): Double = {
  	  val sumSqr = x.toArray.zip(z.toArray)
  	                        .map(xz => (xz._1 - xz._2))
  	                        .map( x => x*x)
  	                        .sum
  	 Math.sqrt(sumSqr)
  }
  
  private def validateDimension(A: DblMatrix,  B: DblMatrix,  H: DblMatrix, P: DblMatrix): Unit = {
  	 require(A != null && H != null && P != null, "Cannot create a Kalman filter with undefined parameters")
     require( A.size ==B.size && A(0).size == B(0).size, s"Incorrect dimension in Kalman filter A(${A.size}x${A(0).size}) and B(${B.size}x${B(0).size})")
     require( A.size == H.size && A(0).size == H(0).size, s"Incorrect dimension in Kalman filter A(${A.size}x${A(0).size}) and H(${H.size}x${H(0).size})")
     require( A.size == P.size && A(0).size == P(0).size, s"Incorrect dimension in Kalman filter A(${A.size}x${A(0).size}) and P(${P.size}x${P(0).size})")
  }

}

		/**
		 * Companion object for the Kalman filter DKalman that defines its constructor.
		 */
object DKalman {
   private def noControl(nCols: Int, nRows: Int): DblMatrix = Array.fill(nCols)(Array.fill(nRows)(0.0))
		   /**
		    * <p>Constructor for the Kalman filter with Control Matrix B</p>
		    * @param A State transition matrix  [
		    * @param B Control state matrix
		    * @param H Matrix that defines the dependency of the measurement on the state of the system
		    * @param P Covariance error matrix
		    * @param qrNoise Implicit value representing the white noise for the process Q and the measurement P
		    * 
		    */
   def apply(A: DblMatrix, B: DblMatrix, H: DblMatrix, P: DblMatrix)(implicit qrNoise: QRNoise): DKalman = new DKalman(A, B, H,P)(qrNoise)
   def apply(A: DblMatrix, H: DblMatrix, P: DblMatrix)(implicit qrNoise: QRNoise): DKalman = new DKalman(A, noControl(A.size, A(0).size), H,P)(qrNoise)
}



// -----------------------  EOF ---------------------------------
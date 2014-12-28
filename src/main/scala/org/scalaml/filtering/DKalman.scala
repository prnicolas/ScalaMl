/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.filtering

	// Scala standard libraary
import scala.annotation.implicitNotFound
import scala.util.{Try, Success, Failure, Random}
	// 3rd party libraries
import org.apache.commons.math3.linear._
import org.apache.commons.math3.filter._
import org.apache.log4j.Logger
	// ScalaMl classes
import org.scalaml.core.XTSeries
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.Design.PipeOperator
import org.scalaml.util.DisplayUtils


		/**
		 * <p>Case class that defines the <b>white (Gaussian)</b> noise <b>q</b> for the process 
		 * and the white noise <b>r</b> for the measurement or observation devices. An instance
		 * of this class has to be provided implicitly in the scope of the Kalman filter
		 * instance (DKalman).</p>
		 * @param qr Tuples that define the mean values of the process and measurement noise.
		 * @param white Scalar function that specify the white noise distribution
		 * @constructor Create an instance of QRNoise with a tuple {mean q noise, mean R noise}. 
		 * @see org.apache.commons.math3.linear
		 * @author Patrick Nicolas
		 * @since February 12, 2014
		 * @note Scala for Machine Learning chapter 3 Data pre-processing / Kalman filter
		 */
case class QRNoise(qr: XY, white: Double=> Double) {
  
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
		 * <p>Compute the white noise of the measurement in Kalman filter</p>
		 * @return Array of two process noise value
		 */
	def noisyQ: DblVector = Array[Double](q, q)

		/**
		 * <p>Compute the white noise of the measurement in Kalman filter</p>
		 * @return Array of two measurement noise value
		 */
	def noisyR: DblVector = Array[Double](r, r)
}


		/**
		 * <p>Kalman filter class that uses a recursive optimal filter. The client code has to 
		 * provide the time independent state transition t to t+1, the input matrix B. the measurement 
		 * dependency matrix, H and the error matrix P. This implementation uses the Apache Commons 
		 * math library. The process and measurement white noise is provided as an implicit value.</p>
		 * @param A State transition matrix
		 * @param B Control state matrix
		 * @param H Matrix that defines the dependency of the measurement on the state of the system<
		 * @param P Covariance error matrix
		 * @param qrNoise Implicit value representing the white noise for the process Q and the measurement P.
		 * @constructor Create a scalar Kalman filter
		 * @throws IllegalArgumentException if the input matrices are undefined or have inconsistent dimension
		 * @throws ImplicitNotFoundException if the white noise is not defined prior instantiation of the DKalman class.
		 * @see org.apache.commons.math3.filter._
		 * @author Patrick Nicolas
		 * @since February 11, 2014
		 * @note Scala for Machine Learning  chapter 3 Data pre-processing / Kalman filter
		 */
@implicitNotFound("White noise, QRNoise, has to be implicitly defined for Kalman filter")
final protected class DKalman(
		A: DblMatrix,  
		B: DblMatrix,  
		H: DblMatrix, 
		P: DblMatrix)
		(implicit qrNoise: QRNoise) extends PipeOperator[XTSeries[XY], XTSeries[XY]] {
	
	import DKalman._
   
	check(A, B,  H, P)
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
		 * @return PartialFunction of time series of elements of type T as input to the Discrete 
		 * Fourier filter and time series of frequencies of type Double as output
		 */
	override def |> : PartialFunction[XTSeries[XY], XTSeries[XY]] = {
		case xt: XTSeries[XY] if( !xt.isEmpty) => xt.map( y => {
			initialize(Array[Double](y._1, y._2))  // Initialize the filter
			val nState = newState				// Extract the new state a two values vector
			(nState(0), nState(1))
		})
	}

  
		/**
		 * Initialize the Kalman filter class of Apache Commons Math with an input of values
		 */
	private def initialize(input: DblVector): Unit = {  	
		val pModel = new DefaultProcessModel(A, B, Q, input, P)
		val mModel = new DefaultMeasurementModel(H, R)
			// Create a Kalman filter with a model pModel for the process
			// and a model mModel for the measurement.
		filter = new KalmanFilter(pModel, mModel)
			// Conversion to Apache Commons Math internal types
		x = new ArrayRealVector(input)
	}
  
		/**
		 * Compute the new state of the Kalman iterative computation
		 */
	private def newState: DblVector = {
		import org.scalaml.core.Types.CommonMath._
		
			// Update the filter with the predictive value for x
			// and update it with the A transition matrix with the process noise qr.Q
		filter.predict
		x = A.operate(x).add(qrNoise.noisyQ) 

			// Compute the measured value z with the new update input value 
			// using the measurement-statement dependency matrix H
		val z = H.operate(x).add(qrNoise.noisyR)
		
			// Update the filter with the new estimated measured value z
		filter.correct(z)
		filter.getStateEstimation	
	}
  
		/**
		 * Compute the least squared errors of two vectors of type 'RealVector'
		 */
	private def lsError(x: RealVector, z: RealVector): Double = {
		val sumSqr = x.toArray.zip(z.toArray)
							.map(xz => (xz._1 - xz._2))
							.map( x => x*x).sum
		Math.sqrt(sumSqr)
	}

}

		/**
		 * Companion object for the Kalman filter DKalman that defines its constructor.
		 * @author Patrick Nicolas
		 * @since February 11, 2014
		 * @note Scala for Machine Learning  chapter 3 Data pre-processing / Kalman filter
		 */
object DKalman {
	private def noControl(nCols: Int, nRows: Int): DblMatrix = 
			Array.fill(nCols)(Array.fill(nRows)(0.0))
		   
		/**
		 * <p>Constructor for the Kalman filter with Control Matrix B</p>
		 * @param A State transition matrix  [
		 * @param B Control state matrix
		 * @param H Matrix that defines the dependency of the measurement on the state of the system
		 * @param P Covariance error matrix
		 * @param qrNoise Implicit value representing the white noise for the process Q and the 
		 * measurement P
		  */
	def apply(A: DblMatrix, B: DblMatrix, H: DblMatrix, P: DblMatrix)(implicit qrNoise: QRNoise): DKalman = 
		new DKalman(A, B, H,P)(qrNoise)
		
		/**
		 * <p>Constructor for the Kalman filter without Control Matrix B</p>
		 * @param A State transition matrix  [
		 * @param H Matrix that defines the dependency of the measurement on the state of the system
		 * @param P Covariance error matrix
		 * @param qrNoise Implicit value representing the white noise for the process Q and the 
		 * measurement P
		  */
	def apply(A: DblMatrix, H: DblMatrix, P: DblMatrix)(implicit qrNoise: QRNoise): DKalman = 
		new DKalman(A, noControl(A.size, A(0).size), H,P)(qrNoise)

  
	private def check(A: DblMatrix,  B: DblMatrix, H: DblMatrix, P: DblMatrix): Unit = {
		require( !A.isEmpty && !H.isEmpty && !P.isEmpty, 
				"DKalman.check Cannot create a Kalman filter with undefined parameters")
		require( A.size ==B.size && A(0).size == B(0).size, 
				s"DKalman.check Incorrect dimension A(${A.size}x${A(0).size}) or B(${B.size}x${B(0).size})")
		require( A.size == H.size && A(0).size == H(0).size, 
				s"DKalman.check Incorrect dimension  A(${A.size}x${A(0).size}) or H(${H.size}x${H(0).size})")
		require( A.size == P.size && A(0).size == P(0).size, 
				s"DKalman.check Incorrect dimension A(${A.size}x${A(0).size}) or P(${P.size}x${P(0).size})")
	}
}

// -----------------------  EOF ---------------------------------
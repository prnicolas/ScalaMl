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
package org.scalaml.filtering.kalman

	// Scala standard library
import scala.annotation.implicitNotFound
import scala.util.{Try, Random}
import scala.collection._

	// 3rd party libraries
import org.apache.commons.math3.linear._
import org.apache.commons.math3.filter._
import org.apache.log4j.Logger
	// ScalaMl classes
import org.scalaml.stats.XTSeries
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.ETransform
import org.scalaml.util.DisplayUtils
import DKalman._, XTSeries._


		/**
		 * Case class that defines the ''white (Gaussian) noise'' '''q''' for the process 
		 * and the white noise '''r''' for the measurement or observation devices. An instance
		 * of this class has to be provided implicitly in the scope of the Kalman filter
		 * instance (DKalman).
		 * @param qr Tuples that define the mean values of the process and measurement noise.
		 * @param profile Scalar function that specify the white noise distribution
		 * @constructor Create an instance of QRNoise with a tuple {mean q noise, mean R noise}. 
		 * @author Patrick Nicolas
		 * @since 0.98 February 12, 2014
		 * @version 0.98.2
		 * @see Scala for Machine Learning chapter 3 "Data pre-processing" Kalman filter
		 */
case class QRNoise(qr: DblPair, profile: Double => Double = normal) {
		/**
		 * Compute the white noise for process Q
		 * @return white noise (stochastic) value for process Q
		 */
	private def q = profile(qr._1) 
  
		/**
		 * Compute the white noise for measurement R
		 * @return white noise (stochastic) value for measurement R
		 */
	private def r = profile(qr._2) 
  
		/**
		 * Compute the white noise of the measurement in Kalman filter
		 * @return Array of two process noise value
		 */
	lazy val noisyQ: DblArray = Array[Double](q, q)

		/**
		 * Compute the white noise of the measurement in Kalman filter
		 * @return Array of two measurement noise value
		 */
	lazy val noisyR: DblArray = Array[Double](r, r)
}

/**
 * Configuration for Kalman
 * @param A State transition matrix
 * @param B Control state matrix
 * @param H Matrix that defines the dependency of the measurement on the state of the system<
 * @param P Covariance error matrix
 */
case class KalmanConfig(A: DblMatrix, B: DblMatrix, H: DblMatrix, P: DblMatrix)


		/**
		 * Kalman filter class that uses a recursive optimal filter. The client code has to 
		 * provide the time independent state transition t to t+1, the input matrix B. the measurement 
		 * dependency matrix, H and the error matrix P. This implementation uses the ''Apache Commons 
		 * math library.'' The process and measurement white noise is provided as an implicit value.
		 * @param config Configuration for Kalman contains the state transition matrix, Control state matrix, Matrix that
		 *               defines the dependency of the measurement on the state of the system and Covariance error matrix
		 * @param qrNoise Implicit value representing the white noise for the process Q and the 
		 * measurement P.
		 * @constructor Create a scalar Kalman filter
		 * @throws IllegalArgumentException if the input matrices are undefined or have inconsistent 
		 * dimension
		 * @author Patrick Nicolas
		 * @since 0.98 February 11, 2014
		 * @version 0.98.3
		 * @see Scala for Machine Learning  chapter 3 Data pre-processing / Kalman filter
		 * @see org.apache.commons.math3.filter._
		 * @see http://apache.org/
		 * @note 0.98.3 update implementation mse with ''aggregate''
		 */
@implicitNotFound(msg = "Kalman White noise has to be implicitly defined")
@throws(classOf[IllegalArgumentException])
final protected class DKalman(config: KalmanConfig)
		(implicit qrNoise: QRNoise) extends ETransform[KalmanConfig](config) {
  
	type U = Vector[DblPair]
	type V = Vector[DblPair]
	type KRState = (KalmanFilter, RealVector)

	check(config.A, config.B, config.H, config.P)
	
	def this(A: DblMatrix, B: DblMatrix, H: DblMatrix, P: DblMatrix)(implicit qrNoise: QRNoise) =
		this(KalmanConfig(A, B, H, P))(qrNoise)
	   
	private val logger = Logger.getLogger("DKalman")
  		
		/**
		 * Process related white noise (mean = 0.0)
		 */	
	private[this] val Q: DblMatrix = new DblMatrix(config.A.length).map(_ => 
		Array.fill(config.A(0).length)(qrNoise.qr._1))

		/**
		 * Measurement related white noise (mean = 0.0)
		 */
	private[this] val R: DblMatrix = new DblMatrix(config.A.length).map(_ => 
		Array.fill(config.A(0).length)(qrNoise.qr._2) )
  
		/**
		 * Overloaded pipe operator to implement the Kalman filter as a data transformation. The
		 * exception thrown by the Apache Commons Math library are internally caught in case of 
		 * computational failure and the data transformation return None.
		 * @throws MatchError if the input time series is undefined or have no elements
		 * @return PartialFunction of time series of elements of type T as input to the Discrete 
		 * Fourier filter and time series of frequencies of type Double as output
		 */
	@throws(classOf[NonSquareMatrixException])
	@throws(classOf[NonPositiveDefiniteMatrixException])
	@throws(classOf[MatrixDimensionMismatchException])
	override def |> : PartialFunction[U, Try[V]] = {
		case xt: U if  xt.nonEmpty => Try(
			xt.map { case (prev, next) =>
		  
				// 1. Initialize the measurement and process models defined in Apache Commons Math
				// 2. Extract the new state a two values vector
				val nState = newState(initialize(prev, next)) //Array[Double](y._1, y._2)) )
				(nState(0), nState(1))
			}
		)
	}
	

  
		/**
		 * Initialize the Kalman filter class of Apache Commons Math with an input of values
		 */
	private def initialize(input: DblArray): KRState = {  

			// Create a Kalman filter with a model pModel for the process
			// and a model mModel for the measurement.
		val pModel = new DefaultProcessModel(config.A, config.B, Q, input, config.P)
		val mModel = new DefaultMeasurementModel(config.H, R)
		(new KalmanFilter(pModel, mModel), new ArrayRealVector(input))
	}
	
	private def initialize(prev: Double, next: Double): KRState = initialize(Array[Double](prev, next))
  
		/**
		 * Compute the new state of the Kalman iterative computation
		 */
	private def newState(state: KRState): DblArray = {
		import org.scalaml.libraries.commonsmath.CommonsMath._
		
			// Update the filter with the predictive value for x
			// and update it with the A transition matrix with the process noise qr.Q
		state._1.predict()
		
		 // Conversion to Apache Commons Math internal types
		val x: RealVector = config.A.operate(state._2).add(qrNoise.noisyQ) 

			// Compute the measured value z with the new update input value 
			// using the measurement-statement dependency matrix H
		val z = config.H.operate(x).add(qrNoise.noisyR)
		
			// Update the filter with the new estimated measured value z
		state._1.correct(z)
		state._1.getStateEstimation	
	}
}

		/**
		 * Companion object for the Kalman filter DKalman that defines its constructor.
		 * @author Patrick Nicolas
		 * @since 0.98 February 11, 2014
		 * @version 0.98.2
		 * @see Scala for Machine Learning  chapter 3 Data pre-processing / Kalman filter
		 */
object DKalman {
	import org.scalaml.stats.Stats
	val normal = Stats.normal(_)  //(x: Double) => Stats.normal(x)
  
	private def noControl(nCols: Int, nRows: Int): DblMatrix = 
			Array.fill(nCols)(Array.fill(nRows)(0.0))
		   
		/**
		 * Constructor for the Kalman filter with Control Matrix B
		 * @param A State transition matrix  [
		 * @param B Control state matrix
		 * @param H Matrix that defines the dependency of the measurement on the state of the system
		 * @param P Covariance error matrix
		 * @param qrNoise Implicit value representing the white noise for the process Q and the 
		 * measurement P
		  */
	def apply(
			A: DblMatrix, 
			B: DblMatrix, 
			H: DblMatrix, 
			P: DblMatrix)(implicit qrNoise: QRNoise): DKalman = new DKalman(A, B, H,P)(qrNoise)
		
		/**
		 * Constructor for the Kalman filter without Control Matrix B
		 * @param A State transition matrix  [
		 * @param H Matrix that defines the dependency of the measurement on the state of the system
		 * @param P Covariance error matrix
		 * @param qrNoise Implicit value representing the white noise for the process Q and the 
		 * measurement P
		  */
	def apply(A: DblMatrix, H: DblMatrix, P: DblMatrix)(implicit qrNoise: QRNoise): DKalman = 
		new DKalman(A, noControl(A.length, A(0).length), H,P)(qrNoise)

  
	private def check(A: DblMatrix,  B: DblMatrix, H: DblMatrix, P: DblMatrix): Unit = {
		require( A.length > 0 && H.length > 0 && P.length > 0, 
				"DKalman.check Cannot create a Kalman filter with undefined parameters")
		require( A.length ==B.length && A(0).length == B(0).length, 
				s"DKalman.check Incorrect dim. A(${A.length}x${A(0).length}) or B(${B.length}x${B(0).length})")
		require( A.length == H.length && A(0).length == H(0).length, 
				s"DKalman.check Incorrect dim.  A(${A.length}x${A(0).length}) or H(${H.length}x${H(0).length})")
		require( A.length == P.length && A(0).length == P(0).length, 
				s"DKalman.check Incorrect dim. A(${A.length}x${A(0).length}) or P(${P.length}x${P(0).length})")
	}
}


// -----------------------  EOF ---------------------------------
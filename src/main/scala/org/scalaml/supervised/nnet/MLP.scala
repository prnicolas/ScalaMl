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
package org.scalaml.supervised.nnet

	// Scala library
import scala.annotation.implicitNotFound
import scala.collection._
import scala.util.{Random, Try, Success, Failure}
	// 3rd party
import org.apache.log4j.Logger
	// ScalaMl classes
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.util.MathUtils._
import org.scalaml.stats.Loss._
import org.scalaml.stats.XTSeries._
import org.scalaml.core.ITransform
import org.scalaml.util.LoggingUtils._
import MLP._

		/**
		 * Implementation of the Multi-layer Perceptron as a Feed-foward Neural Network that follows
		 * the standard design of supervised learning algorithm:
		 * - The classifier implements the '''ITransform''' implicit monadic data transformation
		 * - The constructor triggers the training of the classifier, making the model immutable
		 * - The classifier implements the '''Monitor''' interface to collect profile information for 
		 * debugging purpose
		 * 
		 * Model are created through training during instantiation of the class. The model is 
		 * created even in the training does not converged towards a stable network of synapse weights. 
		 * In this case the client code is responsible for checking that the value of the state variable 
		 * converge and a validation run is performed.
		 * 
		 * The classifier is implemented as a data transformation and extends the ITransform trait.
		 * This implementation uses the stochastic gradient with a momentum factor
		 * 
		 * This MLP uses the online training strategy suitable for time series.
		 * {{{
		 *   Activation function h:  y = h.[w(0) + w(1).x(1) + ... + w(n).x(n)] with  weights wi
		 *   Output layer: h(x) = x
		 *   Hidden layers:  h(x) = 1/(1+exp(-x))
		 *   Error back-propagation for neuron i:
		 *   error(i) = y(i) - w(0) - w(1).x(1) - w(n).x(n)	
		 * }}}
		 * @constructor Instantiates a Multi-layer Perceptron for a specific configuration, time 
		 * series and target or labeled data. 
		 * @throws IllegalArgumentException if the any of the class parameters is undefined
		 * @param config  Configuration parameters class for the MLP
		 * @param xt Time series of features in the training set
		 * @param expected  Labeled or expected observations used for training
		 * @param mode Operating mode or objective of the model (classification or regression)
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98.1 May 8, 2014
		 * @version 0.99.1
		 * @see org.scalaml.core.ITransform
		 * @see org.scalaml.util.Monitor
		 * @see Scala for Machine Learning Chapter 9 ''Artificial Neural Network'' / 
		 * Multilayer perceptron / Training cycle / epoch
		 */
@throws(classOf[IllegalArgumentException])
@implicitNotFound(msg = "MultiLinearRegression Implicit conversion $T to Double undefined")
final class MLP[T <: AnyVal](
		config: MLPConfig, 
		hidden: Array[Int] = Array.empty[Int],
		xt: XVSeries[T], 
		expected: XVSeries[T])
		(implicit mode: MLPMode, f: T => Double) 
	extends ITransform[Array[T]](xt) with Monitor[Double] {

	
	check(xt, expected)
	
		/**
		 * Output type for the predictive function |>
		 */
	type V = DblArray

	protected val logger = Logger.getLogger("MLP")
	
		/**
		 * Initializes the topology of this multi-layer perceptron starting
		 * from the input data (features) to the output layer
		 */
	lazy val topology = 
		if(hidden.length ==0) 
			Array[Int](xt.head.length, expected.head.length)
		else 
			Array[Int](xt.head.length) ++ hidden ++ Array[Int](expected.head.length)
	
	
		/**
		 * Model for the Multi-layer Perceptron of type MLPNetwork. This implementation
		 * allows the model to be created even in the training does not converged towards
		 * a stable network of synapse weights. The client code is responsible for 
		 * evaluating the value of the state variable converge and perform a validation run
		 */
	val model: Option[MLPModel] = train

		
		/**
		 * Test whether the model has converged. .
		 * @return true if the training execution converges, false otherwise
		 */
	@inline
	final def isModel: Boolean = model.isDefined
	
		/**
		 * Define the predictive function of the classifier or regression as a data
		 * transformation by overriding the pipe operator |>.
		 * @throws MatchError if the model is undefined or the input string has an incorrect size
		 * @return PartialFunction of features vector of type Array[T] as input and 
		 * the predicted vector values as output
		 */
	override def |> : PartialFunction[Array[T], Try[V]] = {
		case x: Array[T] if isModel && x.length == dimension(xt) =>
			Try(MLPNetwork(config, topology, model).predict(x))
	}


		/**
		 * Computes the accuracy of the training session. The accuracy is estimated
		 * as the percentage of the training data points for which the square root of 
		 * the sum of squares error, normalized by the size of the  training set exceed a 
		 * predefined threshold.
		 * @param threshold threshold applied to the square root of the sum of squares error to 
		 * validate the data point
		 * @return accuracy value [0, 1] if model exits, None otherwise
		 */
	final def fit(threshold: Double): Option[Double] = model.map(m => 
				
			// counts the number of data points for were correctly classified
		xt.map( MLPNetwork(config, topology, Some(m)).predict(_) )
				.zip(expected)
				.count{ case (y, e) => mse(y, e.map(_.toDouble))< threshold }
				/xt.size.toDouble)

				
		/*
		 * Training method for the Multi-layer perceptron. This method simply manages the
		 * execution of Epochs. The execution within an epoch is performed by the method
		 * ''MLPNetwork.trainEpoch''
		 * 
		 * This implementation uses the stochastic gradient with a momentum factor
		 * @return A MLP model (synapses for the network) if the training converges, None otherwise
		 */
	private def train: Option[MLPModel] = {
		val network = new MLPNetwork(config, topology)
	
			// Apply the exit condition for this online training strategy
			// The convergence criteria selected is the reconstruction error
			// generated during an epoch adjusted to the scaling factor and compare
			// to the predefined criteria config.eps.
		val zi =  xt.toVector.zip(expected)

		var prevErr = Double.MaxValue
		
		Range(0, config.numEpochs).find( n => {			
			val err = Shuffle.fisherYates(xt.size)
											.map(zi(_))
											.map{ case(x, e) => network.trainEpoch(x, e)}
											.sum/xt.size
							
				// Counter for monitoring and statistics
			count("err",err)
			
			val diffErr = err - prevErr
			prevErr = err
			Math.abs(diffErr) < config.eps
		})
		.map(_ => network.getModel)
	}
	

	
	override def toString: String = {
		val modelStr = if(isModel) model.get.toString else "No model"
		s"Topology: ${topology.mkString(" ,")}\n$modelStr"
	}
}


		/**
		 * Companion object for the Multi-layer Perceptron. The singleton is used to:
		 * 
		 * - Define several variants of the constructor
		 * 
		 * - Define the class/trait hierarchy for the objective of the MLP {classification, regression}
		 * @author Patrick Nicolas
		 * @since 0.98.1 May 8, 2014
		 * @version 0.99.1
		 * @see Scala for Machine Learning Chapter 9 ''Artificial Neural Network'' / 
		 * Multilayer perceptron/Training cycle/epoch
		 */
object MLP {
	private val EPS = 1e-5

	
	final val diff = (x: Double, y: Double) => x - y
	
	
		/**
		 * Trait that defined the signature of the objective or operating mode
		 */
	trait MLPMode {
		/**
		 * Normalize the output vector to match the objective of the MLP. The
		 * output vector is the output layers minus the bias, output(0).
		 * @param output raw output vector
		 * @return normalized output vector
		 */
		def apply(output: DblArray): DblArray
		
		def error(labels: DblArray, output: DblArray): Double = mse(labels, output)
	}
	
		/**
		 * Class for the binary classification objective using the Multi-layer perceptron.
		 */
	final class MLPBinClassifier extends MLPMode {
		
		/**
		 * Normalize the output vector to match the objective of the MLP. The output returns
		 * a sigmoid value
		 * @param output raw output vector
		 * @return normalized output vector
		 */
		override def apply(output: DblArray): DblArray = output.map(sigmoid(_))

		override def error(labels: DblArray, output: DblArray): Double = 
			crossEntropy(labels.head, output.head)
	}
	
		/**
		 * Class signature for the Regression objective for the MLP
		 */
	final class MLPRegression extends MLPMode  {
		
		/**
		 * Normalize the output vector to match the objective of the MLP. The
		 * output vector is the output layers minus the bias, output(0).
		 * @param output raw output vector
		 * @return normalized output vector
		 */
		override def apply(output: DblArray): DblArray = output
	}


	
		/**
		 * Class for the Regression objective for the MLP. This implementation uses softmax 
		 */
	final class MLPMultiClassifier extends MLPMode {
				
		/**
		 * Normalize the output vector to match the objective of the MLP. In the case of multinomial
		 * classifier, the 
		 * @param output raw output vector
		 * @return normalized output vector
		 */
		override def apply(output: DblArray): DblArray = softmax(output)
		
		private def softmax(y: DblArray): DblArray = {
			val softmaxValues = new DblArray(y.length)
			val expY = y.map( Math.exp(_) )
			val expYSum = expY.sum
			
			expY.map( _ /expYSum).copyToArray(softmaxValues, 1)
			softmaxValues
		}
	}
	
		/**
		 * Default constructor for the Multi-layer perceptron (type MLP)
		 * @param config  Configuration parameters class for the MLP
		 * @param hidden Array of size of the hidden layers (i.e. Array[Int](4, 6) represents two hidden layers of 4 m
		 *               and 6 nodes
		 * @param xt Time series of features in the training set
		 * @param expected  Labeled or target observations used for training
		 * @param mode mode or Objective of the model (classification or regression)
		 */
	def apply[T <: AnyVal](
			config: MLPConfig, 
			hidden: Array[Int],
			xt: XVSeries[T],
			expected: XVSeries[T])
			(implicit mode: MLP.MLPMode, f: T => Double): MLP[T] =
		new MLP[T](config, hidden, xt, expected)

			/**
			 * Constructor for the Multi-layer perceptron (type MLP) without hidden layer
			 * @param config  Configuration parameters class for the MLP
			 * @param xt Time series of features in the training set
			 * @param expected  Labeled or target observations used for training
			 * @param mode mode or Objective of the model (classification or regression)
			 */
	def apply[T <: AnyVal](
			config: MLPConfig, 
			xt: XVSeries[T],
			expected: XVSeries[T])
			(implicit mode: MLP.MLPMode, f: T => Double): MLP[T] =
		new MLP[T](config, Array.empty[Int], xt, expected)

			/**
			 * Constructor for the Multi-layer perceptron (type MLP) with one hidden layer
			 * @param config  Configuration parameters class for the MLP
			 * @param nHiddenNodes Number of nodes in the single hidden layer
			 * @param xt Time series of features in the training set
			 * @param expected  Labeled or target observations used for training
			 * @param mode mode or Objective of the model (classification or regression)
			 */
	def apply[T <: AnyVal](
			config: MLPConfig, 
			nHiddenNodes: Int,
			xt: XVSeries[T],
			expected: XVSeries[T])
			(implicit mode: MLP.MLPMode, f: T => Double): MLP[T] =
		new MLP[T](config, Array[Int](nHiddenNodes), xt, expected)
		
	def apply[T <: AnyVal](
			config: MLPConfig, 
			hiddenLayers: Array[Int],
			xt: Array[Array[T]],
			expected: Array[Array[T]])
			(implicit mode: MLP.MLPMode, f: T => Double): MLP[T] =
		new MLP[T](config, hiddenLayers, xt.toVector, expected.toVector)

            
	private def check[T](xt: XVSeries[T], labels: XVSeries[T]): Unit = {
		require( xt.nonEmpty,
				"Features for the MLP are undefined")
		require( labels.nonEmpty,
				"Labeled observations for the MLP are undefined")
		require(xt.size == labels.size, 
			s"MLP.check Found xt.size ${xt.size} != label.size ${labels.size} required ==")
	}
}

// ----------------------------------------------  EOF ------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.supervised.nnet

import org.apache.commons.math3.linear.RealMatrix
import org.scalaml.core.types.ScalaMl._
import scala.util.Random
import org.scalaml.core.XTSeries
import org.scalaml.core.design.Config



		/**
		 * <p>Class that defines the configuration for the Multi-layer Perceptron. The validation
		 * of the configuration/tuning parameters for the MLP is defined in this class..<br>
		 * <pre><span style="font-size:9pt;color: #351c75;font-family: &quot;Helvetica Neue&quot;,Arial,Helvetica,sans-serif;">
		 * <b>alpha</b>       Momentum parameter used to adjust the value of the gradient of the weights with previous value (smoothing)
		 * <b>eta</b>         Learning rate ]0, 1] used in the computation of the gradient of the weights during training
		 * <b>hidLayers</b>   Sequence of number of neurons for the hidden layers
		 * <b>numEpochs</b>   Number of epochs or iterations allowed to train the weights/model
		 * <b>eps</b>         Convergence criteria used as exit condition of the convergence toward optimum weights that minimize the sum of squared error		 
		 * <b>activation</b>  Activation function (sigmoid or tanh) that computes the output of hidden layers during forward propagation
		 * </span></pre></p>
		 * @constructor Creates a configuration object for the neural network. [alpha] Momentum parameter used to adjust the value of the gradient of the weights with previous value (smoothing). [eta] Learning rate ]0, 1] used in the computation of the gradient of the weights during training. [hidLayers] Sequence of number of neurons for the hidden layers. [numEpochs] Number of epochs used to train the weights/model. [eps] Convergence criteria used as exit condition of the convergence toward optimum weights that minimize the sum of squared error. [activation] Activation function (sigmoid or tanh) that computes the output of hidden layers during forward propagation
		 * @throws IllegalArgumentException if one of the class parameters is either out of bounds or undefined * 
		 * @author Patrick Nicolas
		 * @since May 7, 2014
		 * @note Scala for Machine Learning Chapter 9 Artificial Neural Network/Multilayer perceptron
		 */
final class MLPConfig(val alpha: Double, val eta: Double, val hidLayers: Array[Int], val numEpochs: Int, val eps: Double, val activation: Double => Double) 
								extends Config {
	import MLPConfig._
	val persists = "config/mlp"
	  
	check(alpha, eta, numEpochs, activation)

		/**
		 * <p>Return the id of the output layer.</p>
		 * @return 1 if there is no hidden layers, the id of the last hidden layer + 1, otherwise
		 */
	final def outLayerId: Int = if(hidLayers == null) 1 else hidLayers.size+1
    
		/**
		 * <p>Return the number of hidden layers in this Neural network.</p>
		 * @return 0 if there is no hidden layer, the size of the hidLayer array or sequence otherwise
		 */
	final def nHiddens: Int = if(hidLayers == null) 0 else hidLayers.size
}



		/**
		 * <p>Companion object for the MLConfig class. This singleton defines the boundary
		 * values for the parameters of the class and the different variation of constructors.</p
		 * 
		 * @author Patrick Nicolas
		 * @since May 4, 2014
		 */
object MLPConfig {
	final val EPS: Double = 1e-17
	final val ALPHA_LIMITS = (0.0, 1.0)
	final val ETA_LIMITS = (1e-5, 1.0)
	final val NUM_EPOCHS_LIMITS = (2, 5000)
   
	def apply(alpha: Double, eta: Double, hiddenLayers: Array[Int], numEpochs: Int, eps: Double, activation: Double => Double): MLPConfig = 
		new MLPConfig(alpha, eta, hiddenLayers, numEpochs, eps, activation)
   

	def apply(alpha: Double, eta: Double, numHiddenNeurons: Int, numEpochs: Int, eps: Double, activation: Double => Double): MLPConfig = 
		new MLPConfig(alpha, eta, Array[Int](numHiddenNeurons), numEpochs, eps, activation)
   
	def apply(alpha: Double, eta: Double, hiddenLayers: Array[Int], numEpochs: Int, eps: Double): MLPConfig = 
		new MLPConfig(alpha, eta, hiddenLayers, numEpochs, eps, (x: Double) => { 1.0/(1.0 + Math.exp(-x))})
   
	def apply(alpha: Double, eta: Double, hiddenLayers: Array[Int], eps: Double): MLPConfig = 
		new MLPConfig(alpha, eta, hiddenLayers, NUM_EPOCHS_LIMITS._2, eps, (x: Double) => { 1.0/(1.0 + Math.exp(-x))})
      
	def apply(alpha: Double, eta: Double, numHiddenNeurons: Int, eps: Double): MLPConfig = 
		new MLPConfig(alpha, eta, Array[Int](numHiddenNeurons), NUM_EPOCHS_LIMITS._2, eps, (x: Double) => {1.0/(1.0 + Math.exp(-x))})
   
   
	private def check(alpha: Double, eta: Double, numEpochs: Int, activation: Double => Double): Unit = {
		require(alpha >= ALPHA_LIMITS._1 && alpha <= ALPHA_LIMITS._2, s"Momentum factor, alpha $alpha is out of bounds")
		require(eta >= ETA_LIMITS._1 && eta <= ETA_LIMITS._2, s"Learning rate eta for the Neural Network $eta is out of range")
		require(numEpochs > 1, s"Number of epoch $numEpochs for the Neural Network should be > 1")
		require(activation != null, "Activation for MLP is undefined")
	}
}


// ----------------------------------------------  EOF ------------------------------------
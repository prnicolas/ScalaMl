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
 * Version 0.99
 */
package org.scalaml.supervised.nnet


import scala.util.Random

import org.apache.commons.math3.linear.RealMatrix

import org.scalaml.core.Types.ScalaMl._
import org.scalaml.core.Design.Config



		/**
		 * Class that defines the configuration for the Multi-layer Perceptron. The validation
		 * of the configuration/tuning parameters for the MLP is defined in this class.
		 * @constructor Creates a configuration object for the neural network. 
		 * @param alpha  Momentum parameter used to adjust the value of the gradient of the weights 
		 * with previous value (smoothing)
		 * @param eta   Learning rate ]0, 1] used in the computation of the gradient of the weights 
		 * during training
		 * @param hidLayers  Sequence of number of neurons for the hidden layers
		 * @param numEpochs  Number of epochs or iterations allowed to train the weights/model
		 * @param eps  Convergence criteria used as exit condition of the convergence toward optimum 
		 * weights that minimize the sum of squared error		 
		 * @param activation Activation function (sigmoid or tanh) that computes the output of hidden 
		 * layers during forward propagation
		 * 
		 * @throws IllegalArgumentException if one of the class parameters is either out of bounds or 
		 * undefined * 
		 * @author Patrick Nicolas
		 * @since May 7, 2014
		 * @note Scala for Machine Learning Chapter 9 Artificial Neural Network/Multilayer perceptron
		 */
final class MLPConfig(
			val alpha: Double, 
			val eta: Double, 
			val numEpochs: Int, 
			val eps: Double, 
			val activation: Double => Double) extends Config {
	import MLPConfig._

	check(alpha, eta, numEpochs)
}



		/**
		 * Companion object for the MLConfig class. This singleton defines the boundary
		 * values for the parameters of the class and the different variation of constructors.</p
		 * 
		 * @author Patrick Nicolas
		 * @since May 4, 2014
		 * @note Scala for Machine Learning Chapter 9 Artificial Neural Network/Multilayer perceptron
		 */
object MLPConfig {
	private val EPS: Double = 1e-17
	private val ALPHA_LIMITS = (0.0, 1.0)
	private val ETA_LIMITS = (1e-5, 1.0)
	private val NUM_EPOCHS_LIMITS = (2, 5000)
 
		/**
		 * Default constructor for the MLP class
		 * @param alpha  Momentum parameter used to adjust the value of the gradient of the weights 
		 * with previous value (smoothing)
		 * @param eta   Learning rate ]0, 1] used in the computation of the gradient of the weights 
		 * during training
		 * @param hidLayers  Sequence of number of neurons for the hidden layers
		 * @param numEpochs  Number of epochs or iterations allowed to train the weights/model
		 * @param eps  Convergence criteria used as exit condition of the convergence toward optimum 
		 * weights that minimize the sum of squared error		 
		 * @param activation Activation function (sigmoid or tanh) that computes the output of hidden 
		 * layers during forward propagation
		 */
	def apply(
				alpha: Double, 
				eta: Double, 
				numEpochs: Int, 
				eps: Double, 
				activation: Double => Double): MLPConfig = 
		new MLPConfig(alpha, eta, numEpochs, eps, activation)


		/**
		 * Constructor for the MLP class with a logistic function as activation
		 * @param alpha  Momentum parameter used to adjust the value of the gradient of the weights 
		 * with previous value (smoothing)
		 * @param eta   Learning rate ]0, 1] used in the computation of the gradient of the weights 
		 * during training
		 * @param hidLayers  Sequence of number of neurons for the hidden layers
		 * @param numEpochs  Number of epochs or iterations allowed to train the weights/model
		 * @param eps  Convergence criteria used as exit condition of the convergence toward optimum 
		 * weights that minimize the sum of squared error		 
		 */
	def apply(
			alpha: Double, 
			eta: Double, 
			numEpochs: Int, 
			eps: Double): MLPConfig = 
		new MLPConfig(alpha, eta, numEpochs, eps, (x:Double) =>{1.0/(1.0 + Math.exp(-x))})

		/**
		 * Constructor for the MLP class with a logistic function as activation and
		 * a predefined number of epochs.
		 * @param alpha  Momentum parameter used to adjust the value of the gradient of the weights 
		 * with previous value (smoothing)
		 * @param eta   Learning rate ]0, 1] used in the computation of the gradient of the weights 
		 * during training
		 * @param hidLayers  Sequence of number of neurons for the hidden layers
		 * @param numEpochs  Number of epochs or iterations allowed to train the weights/model
		 */
	def apply(alpha: Double, eta: Double, eps: Double): MLPConfig = 
		new MLPConfig(alpha, eta, NUM_EPOCHS_LIMITS._2, eps, 
				(x: Double) => { 1.0/(1.0 + Math.exp(-x))})

   
	private def check(alpha: Double, eta: Double, numEpochs: Int): Unit ={
		require(alpha >= ALPHA_LIMITS._1 && alpha <= ALPHA_LIMITS._2, 
				s"Momentum factor, alpha $alpha is out of bounds")
		require(eta >= ETA_LIMITS._1 && eta <= ETA_LIMITS._2, 
				s"Learning rate eta for the Neural Network $eta is out of range")
		require(numEpochs > 1, s"Number of epoch $numEpochs for the Neural Network should be > 1")
	}
}


// ----------------------------------------------  EOF ------------------------------------
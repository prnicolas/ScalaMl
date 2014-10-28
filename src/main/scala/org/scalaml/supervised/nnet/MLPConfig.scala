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
package org.scalaml.supervised.nnet

import org.apache.commons.math3.linear.RealMatrix
import org.scalaml.core.types.ScalaMl._
import scala.util.Random
import org.scalaml.core.XTSeries
import MLPConfig._
import org.scalaml.core.design.Config
import MLPConfig._


		/**
		 * <p>Class that defines the configuration for the Multi-layer Perceptron. The validation
		 * of the configuration/tuning parameters for the MLP is defined in this class. The softmax
		 * output conversion is defined implicitly and is not a parameter for the class.</p>
		 * @param alpha momentum parameter used to adjust the value of the gradient of the weights with previous value (smoothing)
		 * @param eta learning rate ]0, 1] used in the computation of the gradient of the weights during training
		 * @param numEpochs number of epochs or iterations allowed to train the weights/model
		 * @param eps convergence criteria used as exit condition of the convergence toward optimumn weights that minimize the mean squares error		 * @param classification boolean that specifies if this MLP is used for classification or regression (true for classification, false for regression)
		 * @param activation activation function (sigmoid or tanh) that computes the output of hidden layers during forward propagation
		 * @throws IllegalArgumentException if one of the class parameters is either out of bounds or undefined
		 * 
		 * @author Patrick Nicolas
		 * @since May 7, 2014
		 * @note Scala for Machine Learning
		 */
class MLPConfig(val alpha: Double, 
		        val eta: Double, 
		        val hidLayers: Array[Int], 
		        val numEpochs: Int,
		        val eps: Double,
		        val activation: Double => Double) extends Config {
	val persists = "config/mlp"
	  
    validate(alpha, eta, numEpochs, activation)

    final def outLayerId: Int = if(hidLayers == null) 1 else hidLayers.size+1
    
    final def nHiddens: Int = if(hidLayers == null) 0 else hidLayers.size
    

    
	private def validate(alpha: Double, 
			             eta: Double,  
			             numEpochs: Int,
			             activation: Double => Double): Unit = {
		
	   require(alpha >= ALPHA_LIMITS._1 && alpha <= ALPHA_LIMITS._2, "Momentum factor, alpha " + alpha + " is out of bounds")
	   require(eta >= ETA_LIMITS._1 && eta <= ETA_LIMITS._2, "Learning rate eta for the Neural Network " + eta + " is out of range")
       require(numEpochs > 1, "Number of epoch " + numEpochs + " for the Neural Network should be > 1")
       require(activation != null, "Activation for MLP is undefined")
	}
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
}


// ----------------------------------------------  EOF ------------------------------------
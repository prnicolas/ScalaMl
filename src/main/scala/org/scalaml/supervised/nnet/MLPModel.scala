/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95c
 */
package org.scalaml.supervised.nnet

import org.scalaml.core.types.ScalaMl
import scala.util.Random
import org.scalaml.core.design.Model
import scala.collection.mutable.ListBuffer
import ScalaMl._
import MLPLayer._


		/**
		 * <p>Class that define and manage the components of the MLP model. A MLP model
		 * is fully initialized with a configuration, the input and the number of output values.<br>
		 * The components of a MLP model are:<br>
		 * MLPLayer: Layer or array of neuron or elements<br>
		 * MLPSynapse: Synapse or connection between neurons of two consecutive layers<br>
		 * MLPConnection: Container for all the synapses between two layers<br>
		 * The parameters of the class and the arguments of its methods are not validated as the class 
		 * has package scope (protected)</p>
		 * @constructor MLP model created during training. [config] Configuration parameters for the MLP. [input] Input value for the Network, that is the initial value of the input layer. [nOutputs] Size of the output vector
		 * @param config Configuration parameters for the MLP
		 * @param input Input value for the Network, that is the initial value of the input layer
		 * @param nOutputs Size of the output vector
		 * @throws IllegalArgumentException if the class parameters are either undefined or out-of-range
		 * 
		 * @author Patrick Nicolas
		 * @since May 8, 2014
		 * @note Scala for Machine Learning Chapter 9 Artificial Neural Network/Multilayer perceptron/Model definition
		 */

final protected class MLPModel(config: MLPConfig, nInputs: Int, nOutputs: Int)
                                      (implicit val mlpObjective: MLP.MLPObjective) extends Model {
    val persists = "model/mlp"
	private val topology = if( config.nHiddens == 0)
	                          Array[Int](nInputs, nOutputs) 
	                       else 
	                          Array[Int](nInputs) ++ config.hidLayers ++ Array[Int](nOutputs)
	
	private val layers: Array[MLPLayer] = topology.zipWithIndex.map(t => MLPLayer(t._2, t._1+1))
	private val connections: Array[MLPConnection]  = Range(0, layers.size-1).map(n => 
		        new MLPConnection(config, layers(n), layers(n+1))).toArray

		/**
		 * Alias for the input or first layer in the network
		 * @return input layer
		 */
	@inline 
	final def inLayer: MLPLayer = layers.head
	
		/**
		 * Alias for the last layer (output layer) in the network
		 * @return output layer
		 */
	@inline 
	final def outLayer: MLPLayer = layers.last
	   
		/**
		 * <p>Implements the training cycle or training epoch with the 
		 * first 3 of the 4 stages: Forward propagation of input, back propagation
		 * of error and the re-computation of the weight and gradient of the synapses.<br>
		 * It is assumed that the method argument has been validated in the container class MLP.
		 * @param feature new feature or data point used in the training (online or batch training)
		 * @throws IllegalArgumentException if the feature is either undefined or has incorrect size.
		 */
	def trainEpoch(x: DblVector, y: DblVector): Double = {
	   inLayer.set(x)
  	   connections.foreach( _.connectionForwardPropagation)
       val _sse = sse(y)

	   val bckIterator = connections.reverseIterator
	   bckIterator.foreach( _.connectionBackpropagation)
	     
	   connections.foreach( _.connectionUpdate)
	   _sse
	}
	
	
		/**
		 * <p>Compute the mean squares error for the network as the sum
		 * of the mean squares error for each output value.<br>
		 * It is assumed that the method argument has been validated in the container class MLP.</p>
		 * @param label label or target values used in the computation of the mean squares error
		 * @param objective objective function used to compute the output values.
		 * @return sum of the mean squares error of the output layer.
		 */
	@inline
	final def sse(label: DblVector): Double = outLayer.sse(label)

    
		/**
		 * <p>Compute the output values for the network using the forward propagation.
		 * It is assumed that the method argument has been validated in the container class MLP.</p>
		 * @param feature or data point for which the output has to be computed
		 * @return output vector
		 */
    def getOutput(x: DblVector): DblVector = {
 	   inLayer.set(x)
  	   connections.foreach( _.connectionForwardPropagation)
  	   outLayer.output
    }
	 
   
    override def toString: String = {
      val buf = new StringBuilder
      connections.foreach(buf.append(_))
      layers.foreach(buf.append(_))
      buf.toString
   }
}




// -------------------------------------  EOF ------------------------------------------------
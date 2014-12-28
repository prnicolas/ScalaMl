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
package org.scalaml.supervised.nnet

import scala.collection.mutable.ListBuffer
import scala.util.Random

import org.scalaml.core.Types.ScalaMl
import org.scalaml.core.Design.Model
import ScalaMl._, MLPLayer._


		/**
		 * <p>Class that define and manage the components of the MLP model. A MLP model
		 * is fully initialized with a configuration, the input and the number of output values.<br>
		 * The components of a MLP model are:<ul>
		 * <li>MLPLayer: Layer or array of neuron or elements</li>
		 * <li>MLPSynapse: Synapse or connection between neurons of two consecutive layers</li>
		 * <li>MLPConnection: Container for all the synapses between two layers</li>
		 * </ul>
		 * The parameters of the class and the arguments of its methods are not validated as the class 
		 * has package scope (protected)</p>
		 * @throws IllegalArgumentException if the class parameters are either undefined or out-of-range
		 * @param config Configuration parameters for the MLP.
		 * @param nInputs Input value for the Network, that is the initial value of the input layer.
		 * @param nOutputs   Size of the output vector.
		 * @constructor MLP model created during training. 
		 * @see org.scalaml.core.Design.Model
		 * 
		 * @author Patrick Nicolas
		 * @since May 8, 2014
		 * @note Scala for Machine Learning Chapter 9 Artificial Neural Network / Multilayer perceptron 
		 * / Model definition
		 */
final protected class MLPModel(
		config: MLPConfig, 
		nInputs: Int, 
		nOutputs: Int)
		(implicit val mlpObjective: MLP.MLPObjective) extends Model {
	
	import MLPModel._

	check(nInputs, nOutputs)
	  
		/*
		 * Create the topology of this Multi-layer preceptron using the configuration 
		 * parameters and the number of hidden layers (and number of elements per hidden layer)
		 */
	private[this] val topology =	if(config.nHiddens == 0) Array[Int](nInputs, nOutputs) 
			else Array[Int](nInputs) ++ config.hidLayers ++ Array[Int](nOutputs)
	
			/*
			 * Create the array of layer using the topology 
			 */
	private[this] val layers: Array[MLPLayer] = topology.zipWithIndex
			.map(t => MLPLayer(t._2, t._1+1))
	
			/*
			 * Create a array of connection between layer. A connection is 
			 * made of multiple synapses.
			 */
	private[this] val connections = Range(0, layers.size-1).map(n => 
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
			// Initialize the input layer
		inLayer.set(x)
			// Apply the forward progapation of input to all the connections
			// starting with the input layer
		connections.foreach( _.connectionForwardPropagation)

			// Compute the sum of squared errors
		val _sse = sse(y)
		
			// Create a back iterator
		val bckIterator = connections.reverseIterator
			
			// Apply the error back propagation to all the connections
			// starting with the output lauer
		bckIterator.foreach( _.connectionBackpropagation)
		
			// Finally update the connections (weigths and grad weights) of synapses
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
		require( !x.isEmpty, "MLPModel.getOutput Input values undefined")
		inLayer.set(x)
		
			// Apply the forward propagation with an input vector ...
		connections.foreach( _.connectionForwardPropagation)
		
			// .. and return the output of the MLP.
		outLayer.output
	}
	
		/**
		 * Write the content of this model (weights) into a file
		 * @return true if the model/weights were saved into file, false otherwise.
		 */
	override def >> : Boolean = {
		val content = new StringBuilder(s"$nInputs,")
		if( config.nHiddens != 0)
			content.append(config.hidLayers.mkString(","))
			
		content.append(s"$nOutputs\n")
		connections.foreach(c => {
			content.append(s"${c.getLayerIds._1},${c.getLayerIds._2}:")
			content.append(c.getSynapses.map(s => s"${s.mkString(",")}\n")) 
		})
		write(content.toString)
	}
	 
		/**
		 * Textual description of the model for Multi-layer Perceptron. The representation
		 * include the description of the connections and layers.
		 */
	override def toString: String = {
		val buf = new StringBuilder
		connections.foreach(buf.append(_))
		layers.foreach(buf.append(_))
		buf.toString
	}
}


		/**
		 * Companion object for a Multi-layer perceptron model. This singleton is used
		 * to validate the class parameters and define its constructors
		 * 
		 * @author Patrick Nicolas
		 * @since May 8, 2014
		 * @note Scala for Machine Learning Chapter 9 Artificial Neural Network / Multilayer perceptron 
		 * / Model definition
		 */
object MLPModel {
	private val MAX_MLP_NUM_INPUTS = 4096
	private val MAX_MLP_NUM_OUTPUTS = 2048
	
	private def check(nInputs: Int, nOutputs: Int): Unit = {
		require(nInputs > 0 && nInputs < MAX_MLP_NUM_INPUTS, 
				s"MLPModel number of input nodes $nInputs is out of range")
		require(nOutputs > 0 && nOutputs < MAX_MLP_NUM_OUTPUTS, 
				s"MLPModel number of output nodes $nOutputs is out of range")
	}
}

// -------------------------------------  EOF ------------------------------------------------
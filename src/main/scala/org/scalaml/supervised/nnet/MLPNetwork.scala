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

import org.scalaml.core.Types.ScalaMl
import org.scalaml.stats.XTSeries._
import ScalaMl._, MLP._


		/**
		 * Class that define and manage the components of the MLP model. A MLP model
		 * is fully initialized with a configuration, the input and the number of output values.
		 * 
		 * The components of a MLP model are:
		 * 
		 * - ''MLPLayer'': Layer or array of neuron or elements
		 * 
		 * - ''MLPSynapse'': Synapse or connection between neurons of two consecutive layers
		 * 
		 * - ''MLPConnection'': Container for all the synapses between two layers
		 *
		 * The parameters of the class and the arguments of its methods are not validated as the class 
		 * has package scope (protected)
		 * @throws IllegalArgumentException if the class parameters are either undefined or out-of-range
		 * @param config Configuration parameters for the MLP.
		 * @param topology Topology {input, hidden and output layer} for the MLP
		 * @param model if the MLP has already been created
		 * @constructor MLP model created during training. 
		 * 
		 * @author Patrick Nicolas
		 * @since 0.99 July 13, 2015
		 * @see Scala for Machine Learning Chapter 9 ''Artificial Neural'' Network / Multilayer perceptron 
		 * / Model definition
		 */
final protected class MLPNetwork(
		config: MLPConfig, 
		topology: Array[Int],
		model: Option[MLPModel] = None)
		(implicit mode: MLP.MLPMode)  {

	MLPNetwork.check(topology)


		/**
		 * Return the number of hidden layers in this Neural network.
		 * @return 0 if there is no hidden layer, the size of the hidLayer array or sequence otherwise
		 */
	final def nHiddens: Int = topology.length -2
	  
	final def output = layers.last.output
	
			/*
			 * Create the array of layer using the topology 
			 */
	private[this] val layers= topology.zipWithIndex.map{ case(t, n) => 
		if(topology.length != n+1) MLPLayer(n, t+1, config.activation) else MLPOutLayer(n, t) }
			
	
			/*
			 * Create a array of connection between layer. A connection is 
			 * made of multiple synapses.
			 */
	private[this] val connections = zipWithShift1(layers).map{ case(src, dst) => 
		new MLPConnection(config, src, dst, model) }

	   
		/**
		 * Implements the training cycle or training epoch with the 
		 * first 2 of the 3 stages: Forward propagation of input, back propagation
		 * of error and the re-computation of the weight and gradient of the synapses.
		 * 
		 * 
		 * It is assumed that the method argument has been validated in the container class MLP.
		 * @param x new feature or data point used in the training (online or batch training)
		 * @param y Expected values for output layer
		 * @throws IllegalArgumentException if the feature is either undefined or has incorrect size.
		 */
	def trainEpoch(x: DblArray, y: DblArray): Double = {
			// Initialize the input layer
		layers.head.setInput(x)
		
			// Apply the forward progapation of input to all the connections
			// starting with the input layer
		connections.foreach( _.connectionForwardPropagation())
		
		
			// Compute the cumulative error for the output layer. The loss function is
			// sum of squared errors for the regression and multinomial classification
			// and the cross-entropy for the binomial classification
		val err = mode.error(y, layers.last.output)

			// Create a back iterator
		val bckIterator = connections.reverseIterator
			
			// Apply the error back propagation to all the connections
			// starting with the output layer
		
		var delta = Delta(zipToArray(y, layers.last.output)(diff))
		bckIterator.foreach( iter => delta = iter.connectionBackpropagation(delta)) 
		err
	}
	
		/**
		 * Retrieve the current model for the neural network
		 * @return The current MLP model
		 */
	final def getModel: MLPModel = {
		val allSynapses = Vector.tabulate(connections.length)(connections(_).getSynapses)
		new MLPModel(allSynapses)
	}
	

    
		/**
		 * Compute the output values for the network using the forward propagation.
		 * It is assumed that the method argument has been validated in the container class MLP.
		 * @param x Data point for which the output has to be computed
		 * @return output vector
		 */
	 def predict(x: DblArray): DblArray = {
		require( x.length > 0, "MLPNetwork.predict Input values undefined")
		require( x.length == topology.head, 
			s"MLPNetwork.predict found x.length ${x.length} required ${topology.head}")

		layers.head.setInput(x)
		
			// Apply the forward propagation with an input vector ...
		connections.foreach( _.connectionForwardPropagation())
		
			// .. and return the output of the MLP without the first (bias) element
		layers.last.output
	}
	
		/**
		 * Textual description of the model for Multi-layer Perceptron. The representation
		 * include the description of the connections and layers.
		 */
	override def toString: String = 
		s"""${connections.map(_.toString).mkString("\n")} 
		| ${layers.map(_.toString).mkString("\n")}""".stripMargin
}


		/**
		 * Companion object for a Multi-layer perceptron model. This singleton is used
		 * to validate the class parameters and define its constructors
		 * 
		 * @author Patrick Nicolas
		 * @since May 8, 2014
		 * @note Scala for Machine Learning Chapter 9 ''Artificial Neural Network'' / Multilayer perceptron 
		 * / Model definition
		 */
object MLPNetwork {
	private val MAX_MLP_LAYERS = 64
	
	def apply(config: MLPConfig, 
			topology: Array[Int],
			model: Option[MLPModel] = None)
			(implicit mlpObjective: MLP.MLPMode): MLPNetwork = 
		new MLPNetwork(config, topology, model)
	
	
	private def check(topology: Array[Int]): Unit = {
		require(topology.length > 1 && topology.length < MAX_MLP_LAYERS, 
			s"MLPNetwork found ${topology.length} layers, require  1 < < $MAX_MLP_LAYERS")
		require(topology.exists( _ > 0), 
			s"MLPNetwork found ${topology.mkString(",")} require number of nodes > 0")
	}
}

// -------------------------------------  EOF ------------------------------------------------
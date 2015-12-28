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

import scala.util.Random

import org.scalaml.core.Types.emptyString
import org.scalaml.stats.XTSeries._
import org.scalaml.util.{FormatUtils, MathUtils}
import MLPModel._, FormatUtils._



		/**
		 * Class that defines the connection between two sequential layers in a Multi-layer Perceptron. 
		 * A layer can be
		 * - Input values
		 * - Hidden 
		 * - Output values 
		 * 
		 * The connections are composed of all the synapses between 
		 * any neuron or variable of each layer.The Synapse is defined as a nested tuple(Double, Double) 
		 * tuple (weights, delta_Weights)
		 * @constructor Create a MLP connection between two consecutive neural layer. 
		 * @param config  Configuration for the Multi-layer Perceptron.
		 * @param src  Source (or input or upstream) neural layer to this connection
		 * @param dst  Destination (or output or downstream) neural layer for this connection.
		 * @param  mode Operating mode or objective of the Neural Network (binary classification, 
		 * regression...)
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98.1 May 5, 2014
		 * @version 0.99.1
		 * @see Scala for Machine Learning Chapter 9 ''Artificial Neural Network'' / Multilayer 
		 * perceptron / Model definition
		 */
final protected class MLPConnection(
		config: MLPConfig, 
		src: MLPLayer, 
		dst: MLPLayer,
		model: Option[MLPModel])
		(implicit mode: MLP.MLPMode) {
	import MLPConnection._

	
		/*
		 * Initialize the matrix (Array of Array) of Synapse by generating
		 * a random value between 0 and ''boundary''. The boundary is computed as the inverse
		 * of the square root of the number of output values + 1 (bias element). 
		 * 
		 * @note The constant BETA may have to be changed according to the type of data.
		 */
	private[this] var synapses: MLPConnSynapses = 
		if(model.isDefined) {
			val boundary = BETA/Math.sqrt(src.output.length+1.0)
			Array.fill(dst.numNonBias)(Array.fill(src.numNodes)((Random.nextDouble*boundary, 0.00)))
		}
		else 
			model.get.synapses(src.id)

		
		/**
		 * Implement the forward propagation of input value. The output
		 * value depends on the conversion selected for the output. If the output or destination
		 * layer is a hidden layer, then the activation function is applied to the dot product of
		 * weights and values. If the destination is the output layer, the output value is just 
		 * the dot product weights and values.
		 */
	def connectionForwardPropagation(): Unit = {
			
			// Iterates over all the synapses, compute the dot product of the output values
			// and the weights and applies the activation function
		val _output = synapses.map(x => dst.activation( inner(src.output, x.map(_._1)) ) )
		
			// Apply the objective function (SoftMax,...) to the output layer
		dst.setOutput(_output)
	}

		/**
		 * Access the identifier for the source and destination layers
		 * @return tuple (source layer id, destination layer id)
		 */
	@inline
	final def getLayerIds: (Int, Int) = (src.id, dst.id)
	
	@inline
	final def getSynapses: MLPConnSynapses = synapses

	
	
		/**
		 * Implement the back propagation of output error (target - output). The method uses
		 * the derivative of the logistic function to compute the delta value for the output of 	
		 * the source layer.
		 * @param delta Delta error from the downstream connection (Connection with the destination
		 * layer as a source).
		 * @return A new delta error to be backpropagated to the upstream connection through the source
		 * layer.
		 */
	def connectionBackpropagation(delta: Delta): Delta = {
		val inputSynapses = if( delta.synapses.length > 0) delta.synapses else synapses
		
				// Invoke the destination layer to compute the appropriate delta error
		val connectionDelta = dst.delta(delta.loss, src.output, inputSynapses)
		
				// Traverses the synapses and update the weights and gradient of weights
		val oldSynapses = synapses.zipWithIndex.map{
			case (synapsesj, j) => synapsesj.zipWithIndex.map{
				case ((w, dw), i) =>
					val ndw = config.eta*connectionDelta.delta(j)(i)
					(w + ndw - config.alpha*dw, ndw)
			}
		}
		synapses = oldSynapses
		
				// Return the new delta error for the next (upstream) connection if any.
		new Delta(connectionDelta.loss, connectionDelta.delta, synapses)
	}

		/**
		 * Textual representation of this connection. The description list the 
		 * values of each synapse as a pair (weight, delta weight)
		 */
	override def toString: String = {
		val descr = Range(0, dst.numNodes).map( i => {
		  
			Range(0, src.numNodes).map(j => {
				val wij: MLPSynapse = synapses(i)(j)
				val weights_str = format(wij._1, emptyString, MEDIUM)
				val dWeights_str = format(wij._2, emptyString, MEDIUM)
				s"$i,$j: ($weights_str, $dWeights_str)  "
			})
			.mkString("\n")
		})
		.mkString("\n")
		
		s"\nConnections weights from layer ${src.id} to layer ${dst.id}\n $descr"
	}
}



		/**
		 * Companion object for the connection of Multi-layer perceptron.
		 * @author Patrick Nicolas
		 * @since 0.98.1 May 5, 2014
		 * @see Scala for Machine Learning Chapter 9 ''Artificial Neural Network'' / Multilayer 
		 * perceptron / Model definition
		 */
object MLPConnection {
  	/**
  	 * Dumping factor for the random initialization of weightss
  	 */
	final val BETA = 0.2
	
		/**
		 * Constructor for an ''MLPConnection''
		 * @param config  Configuration for the Multi-layer Perceptron.
		 * @param src  Source (or input or upstream) neural layer to this connection
		 * @param dst  Destination (or output or downstream) neural layer for this connection.
		 * @param  mode Operating mode or objective of the Neural Network (binary classification, 
		 * regression...)
		 */
	def apply(config: MLPConfig, 
			src: MLPLayer, 
			dst: MLPLayer,
			model: Option[MLPModel] = None)
			(implicit mode: MLP.MLPMode): MLPConnection =
		new MLPConnection(config, src, dst, model)

}

// -------------------------------------  EOF ------------------------------------------------
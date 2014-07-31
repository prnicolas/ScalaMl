/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.supervised.nnet

import org.scalaml.core.Types.ScalaMl
import scala.util.Random
import org.scalaml.supervised.Model
import scala.collection.mutable.ListBuffer



		/**
		 * <p>Class that define and manage the components of the MLP model. A MLP model
		 * is fully initialized with a configuration, the input and the number of output values.<br>
		 * The components of a MLP model are:<br>
		 * MLPLayer: Layer or array of neuron or elements<br>
		 * MLPSynapse: Synapse or connection between neurons of two consecutive layers<br>
		 * MLPConnection: Container for all the synapses between two layers<br>
		 * The parameters of the class and the arguments of its methods are not validated as the class 
		 * has package scope (protected)</p>
		 * @param config Configuration parameters for the MLP
		 * @param input Input value for the Network, that is the initial value of the input layer
		 * @param numOutputs Size of the output vector
		 * @exception IllegalArgumentException if the class parameters are either undefined or out-of-range
		 * 
		 * @author Patrick Nicolas
		 * @date May 8, 2014
		 * @project Scala for Machine Learning
		 */
import ScalaMl._
import MLPLayer._
final protected class MLPModel(val config: MLPConfig, val input: DblVector, val numOutputs: Int) extends Model {
	private val topology = if( config.hiddenLayers == null )
	                          Array[Int](input.size, numOutputs) 
	                       else 
	                          Array[Int](input.size) ++ config.hiddenLayers ++ Array[Int](numOutputs)
	
	private val layers = topology.zipWithIndex.map(t => MLPLayer(t._2, t._1+1))
	private val connections  = Range(0, layers.size-1).map(n => new MLPConnection(config, layers(n), layers(n+1))).toArray

		/**
		 * Alias for the input or first layer in the network
		 * @return input layer
		 */
	@inline 
	final def inputLayer: MLPLayer = layers.head
	
		/**
		 * Alias for the last layer (output layer) in the network
		 * @return output layer
		 */
	@inline 
	final def outputLayer: MLPLayer = layers.last
	   
		/**
		 * <p>Implements the training cycle or training epoch with the 
		 * first 3 of the 4 stages: Forward propagation of input, back propagation
		 * of error and the re-computation of the weight and gradient of the synapses.<br>
		 * It is assumed that the method argument has been validated in the container class MLP.
		 * @param feature new feature or data point used in the training (online or batch training)
		 * @exception IllegalArgumentException if the feature is either undefined or has incorrect size.
		 */
	def trainEpoch(feature: DblVector): Unit = {
	   inputLayer.setInput(feature)
  	   connections.foreach( _.inputForwardPropagation)

	   val bckIterator = connections.reverseIterator
	   bckIterator.foreach( _.errorBackpropagation)
	     
	   connections.foreach( _.updateSynapses)
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
	def mse(label: DblVector, objective: MLP.MLPObjective): Double = outputLayer.mse(label, objective, config.gamma)

    
		/**
		 * <p>Compute the output values for the network using the forward propagation.
		 * It is assumed that the method argument has been validated in the container class MLP.</p>
		 * @param feature or data point for which the output has to be computed
		 * @return output vector
		 */
    def getOutput(feature: DblVector): DblVector = {
 	   inputLayer.setInput(feature)
  	   connections.foreach( _.inputForwardPropagation)
  	   outputLayer.output
    }
	 
   
    override def toString: String = {
      val buf = new StringBuilder
      connections.foreach(buf.append(_))
      layers.foreach(buf.append(_))
      buf.toString
   }
}




// -------------------------------------  EOF ------------------------------------------------
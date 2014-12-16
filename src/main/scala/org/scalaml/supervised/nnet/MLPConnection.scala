/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.2
 */
package org.scalaml.supervised.nnet

import scala.util.Random
import scala.collection.mutable.ListBuffer

import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.supervised.nnet.MLPConfig._
import org.scalaml.core.design.Model
import org.scalaml.supervised.nnet.MLP.MLPObjective
import org.scalaml.util.FormatUtils

		/**
		 * <p>Class that defines the connection between two consecutive (or sequential layers) 
		 * in a Multi-layer Perceptron. The connections is composed of all the synapses between 
		 * any neuron or variable of each layer.The Synapse is defined as a nested tuple(Double, Double) 
		 * tuple (weights, deltaWeights)</p>
		 * @constructor Create a MLP connection between two consecutive neural layer. 
		 * @param config  Configuration for the Multi-layer Perceptron.
		 * @param src  Source (or input or upstream) neural layer to this connection
		 * @param dst  Destination (or output or downstream) neural layer for this connection.
		 * @param  mlpObjective Objective of the Neural Network (binary classification, regression...)
		 * @author Patrick Nicolas
		 * @since May 5, 2014
		 * @note Scala for Machine Learning Chapter 9 Artificial Neural Network / Multilayer perceptron / Model definition
		 */
final protected class MLPConnection(
		config: MLPConfig, 
		src: MLPLayer, 
		dst: MLPLayer)
		(implicit mlpObjective: MLP.MLPObjective)  {
	import MLPConnection._

		/**
		 * <p>Type of Synapse defined as a tuple of [weight, gradient(weights)]
		 */
	type MLPSynapse = (Double, Double)

    
	private val synapses: Array[Array[MLPSynapse]] = Array.tabulate(dst.len)( n => 
			if(n > 0) Array.fill(src.len)((Random.nextDouble*BETA, 0.0)) 
			else Array.fill(src.len)((1.0, 0.0)))
		   
		/**
		 * <p>Implement the forward propagation of input value. The output
		 * value depends on the conversion selected for the output. If the output or destination
		 * layer is a hidden layer, then the activation function is applied to the dot product of
		 * weights and values. If the destination is the output layer, the output value is just 
		 * the dot product weights and values.</p>
		 */
	def connectionForwardPropagation: Unit = {
		val _output = synapses.drop(1).map(x => {
			val sum = x.zip(src.output).foldLeft(0.0)((s, xy) => s + xy._1._1 * xy._2)
		    
			if(!isOutLayer) 
				config.activation(sum)
			else 
				sum
		})
		val out = if(isOutLayer) 
			mlpObjective(_output) 
		else 
			_output
		out.copyToArray(dst.output, 1)     
	}


		/**
		 * <p>Implement the back propagation of output error (target - output). The method uses
		 * the derivative of the logistic function to compute the delta value for the output of 	
		 * the source layer.</p>
		 */
	def connectionBackpropagation: Unit =  
		Range(1, src.len).foreach(i => {
			val err = Range(1, dst.len).foldLeft(0.0)((s, j) => 
					s + synapses(j)(i)._1*dst.delta(j) )
					
				// The delta value is computed as the derivative of the 
				// output value adjusted for the backpropaged error, err
			src.delta(i) =  src.output(i)* (1.0- src.output(i))*err
		})


			/**
			 * <p>Implement the update of the synapse (weight, grad weight) following the
			 * back propagation of output error. This method is called during training.</p>
			 */
	def connectionUpdate: Unit =  
		Range(1, dst.len).foreach(i => {  
			val delta = dst.delta(i)
			
			Range(0, src.len).foreach(j => { 
				val _output = src.output(j)
				val oldSynapse = synapses(i)(j)
				val grad = config.eta*delta*_output
				val deltaWeight = grad + config.alpha*oldSynapse._2
				synapses(i)(j) = (oldSynapse._1 + deltaWeight, grad)
			})
		}) 

	   
	override def toString: String = {
		val buf = new StringBuilder
		buf.append(s"\nConnections weights from layer ${src.id} to layer ${dst.id}\n")

		Range(0, dst.len).foreach( i => {
			Range(0, src.len).foreach(j => {
				val wij: (Double, Double) = synapses(i)(j)
				val weights_str = FormatUtils.format(wij._1, "", FormatUtils.MediumFormat)
				val dWeights_str = FormatUtils.format(wij._2, "", FormatUtils.MediumFormat)
				buf.append(s"$i,$j: ($weights_str, $dWeights_str)  ")
			})
			buf.append("\n")
		})
		buf.toString
	}
	
	
	private def update(i: Int, j: Int, x: Double, dx: Double): Unit = {
		val old = synapses(i)(j)
		synapses(i)(j) = (old._1 + x, dx)
	}
   
	private def isOutLayer: Boolean = dst.id == config.outLayerId
}



		/**
		 * Companion object for the connection of Multi-layer perceptron.
		 * @author Patrick Nicolas
		 * @since May 5, 2014
		 * @note Scala for Machine Learning Chapter 9 Artificial Neural Network / Multilayer perceptron 
		 * / Model definition
		 */
object MLPConnection {
	private val BETA = 0.1
}
// -------------------------------------  EOF ------------------------------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 */
package org.scalaml.supervised.nnet

import org.scalaml.core.Types.ScalaMl
import scala.util.Random
import org.scalaml.supervised.Model
import scala.collection.mutable.ListBuffer



		/**
		 * <p>Class that defines the connection between two consecutive layers in a Multi-layer
		 * Perceptron. The connections is composed of all the synapses between any neuron
		 * or variable of each layer.<br>
		 * The Synapse is defined as a nested class</p>
		 * @param config configuration for the Multi-layer Perceptron
		 * @param src source or ingress layer for the network
		 * @param dst destination of egress layer for the network
		 * @throws IllegalArgumenException if either the configuration or any of the Neural Network layer is defined
		 * 
		 * @author Patrick Nicolas
		 * @since May 5, 2014
		 * @note Scala for Machine Learning
		 */
import ScalaMl._
import MLPConnection._
protected class MLPConnection(val config: MLPConfig, val src: MLPLayer, val dst: MLPLayer)  {
	require(config != null, "Configuration for the MLP connection is undefined")
	require(src != null && dst != null, "The source or destination layer for this connection is undefined")
	
    type MLPSynapse = (Double, Double)
	val synapses: Array[Array[MLPSynapse]] = Array.tabulate(dst.len)( n => if(n > 0) Array.fill(src.len)((beta*Random.nextDouble, 0.0)) else Array.fill(src.len)((1.0, 0.0)))

		   
		/**
		 * <p>Implement the forward propagation of input value. The output
		 * value depends on the conversion selected for the output (identity
		 * or softmax).
		 */
	def inputForwardPropagation: Unit =  {
	  val _output = synapses.drop(1).map(x => {
	  	 val sum = x.zip(src.output).foldLeft(0.0)((s, xy) => s + xy._1._1 * xy._2)
	  	  config.activation(sum)
	  })
	  if( isOutputLayer ) 
	  	 conversion(_output, dst.output)
	  else 
	  	 _output.copyToArray(dst.output, 1)
   }

	   
	  	/**
	  	 * <p>Implement the back propagation of output error (target - output).</p>
	  	 */
	def errorBackpropagation: Unit =  
	   Range(1, src.len).foreach(i => {
	  	   val err = Range(1, dst.len).foldLeft(0.0)((s, j) => s + synapses(j)(i)._1*dst.errors(j) )
	  	   src.errors(i) =  config.gamma*src.output(i)* (1.0- src.output(i))*err
	   })

	  	  
	   
	   	/**
	   	 * Implement the update of the synapse (weight, grad weight) following the
	   	 * back propagation of output error. This method is called during training.
	   	 */
	def updateSynapses: Unit =  
	   Range(1, dst.len).foreach(i => {  
	  	  val msError = dst.errors(i)
	  	  	  
	  	  Range(0, src.len).foreach(j => { 
	  	  	 val out = src.output(j)
	  	  	 val dWeight = synapses(i)(j)._2
	  	  	 val grad = config.eta*msError*out
	  	  	 val deltaWeight = grad + config.alpha*dWeight
	  	  	 update(i,j, deltaWeight, grad)
	  	  })
	   }) 

	   
   override def toString: String = {
	  val buf = new StringBuilder("\nConnections weights from layer ")
	  buf.append(src.id).append(" to layer ").append(dst.id).append("\n")

	  Range(0, dst.len).foreach( i => {
	  	Range(0, src.len).foreach(j => {
	  	   val wij: (Double, Double) = synapses(i)(j)
	  	   buf.append(i + "," + j + ": (" + wij._1 + "," +  wij._2 + ")")
	  	})
	  	buf.append("\n")
	  })
	  buf.toString
   }
	
	
   private def update(i: Int, j: Int, x: Double, dx: Double): Unit = {
	  val old = synapses(i)(j)
	  synapses(i)(j) = (old._1 + x, dx)
   }
   
   @inline
   private def isOutputLayer: Boolean = dst.id == config.outputLayerId
}




		/**
		 * <p>Companion object for MLP connections. The singleton is used to implicitly
		 * defined the softmax and identity function fo the conversion of the output values.
		 * @author Patrick Nicolas
		 * @since May 6, 2014
		 */
object MLPConnection {
	final val beta = 0.6
	implicit def setSoftmax: Unit = conversion = softmax
	implicit def setNoSoftmax: Unit = conversion = noSoftmax
	
	var conversion = (input: DblVector, output: DblVector) => ()
	
    private def softmax(input: DblVector, output: DblVector): Unit = {
	   val softOutput = input.map( Math.exp(_))
	   val softMaxValue = softOutput.max
	   softOutput.map( _ /softMaxValue).copyToArray(output , 1)
	}
	
    private def noSoftmax(input: DblVector, output: DblVector): Unit = input.copyToArray(output, 1)
}

// -------------------------------------  EOF ------------------------------------------------
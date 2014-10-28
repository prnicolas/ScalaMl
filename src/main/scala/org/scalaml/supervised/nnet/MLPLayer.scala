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

import org.scalaml.core.types.ScalaMl
import scala.util.Random
import org.scalaml.core.design.Model
import scala.collection.mutable.ListBuffer
import ScalaMl._
import MLP._

		/**
		 * <p>:Class that defines a MLP layer. A MLP layer is built using the
		 * input vector and add an extra element (or neuron) to account for the intercept
		 * weight w0. The MLP layer is fully defined by its rank in the Neuron Network with
		 * input layer having id = 0 and the output layer having id = number of layers -1.</p>
		 * @param id id or rank of the MLP layer in the network
		 * @param len number of elements or neuron in the MLP layer
		 * @throws IllegalArgumentException if the class parameters are incorrect
		 * 
		 * @author Patrick Nicoloas
		 * @since May 6, 2014
		 * @note Scala for Machine Learning
		 */

protected class MLPLayer(val id: Int, val len: Int) {
   require(id >= 0, "Create a MLP layer with incorrect id: " + id)
   require(len > 0, "Create a MLP layer with zero element")
   
   val output = new DblVector(len) // used for forward propagation
   output.update(0, 1.0)
   val delta = new DblVector(len)  // used for back propagation


   
   		/**
   		 * <p>Initialize the value of the input for this MLP layer.</p>
   		 * @param _input input vector for this layer. 
   		 * @throws IllegalArgumentException if the input vector is undefined
   		 */
   def set(_x: DblVector): Unit = {
  	 require(_x != null, "Cannot initialize this MLP layer " + id  + " with undefined data")
  	 _x.copyToArray(output, 1)
   }


   		/**
   		 * <p>Compute the mean squares error of the neurons/elements of this MLP layer.</p>
   		 * @param labels target output value
   		 * @param objective function used to compute the error = target - output
   		 * @param gamma gamma or gain of the logistic or tanh function
   		 * 
   		 */
   def sse(labels: DblVector): Double = {
  	  require(labels != null, "Cannot compute the mean square error for a MLP layer with undefined labeled values")
  	  require(output.size == labels.size+1, "The size of the output " + output.size + " should be equal to target + 1" + labels.size+1)
  	  
	  var _sse = 0.0
	  output.drop(1)
	        .zipWithIndex
	        .foreach(on => {
  	  	 val err = labels(on._2) - on._1
	     delta.update(on._2+1, on._1* (1.0- on._1)*err)
	  	 _sse += err*err
	  })
	  _sse*0.5
   }
   
   
   @inline
   def isOutput(lastId: Int): Boolean = id == lastId
	  	  
	
   override def toString: String = {
	  val buf = new StringBuilder("\nLayer:")
	  buf.append(id).append(" output: ")
	  output.foreach(buf.append( _).append(","))
	  buf.setCharAt(buf.length-1, ' ')
	  buf.toString
   }
}



		/**
		 * Companion object for the MLP layer used to define a default constructor
		 * @author Patrick Nicolas
		 * @since May 5, 2014
		 */
object MLPLayer {
  def apply(id: Int, len: Int): MLPLayer = new MLPLayer(id, len)
}



// -------------------------------------  EOF ------------------------------------------------
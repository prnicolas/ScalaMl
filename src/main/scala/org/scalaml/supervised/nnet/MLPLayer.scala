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
		 * <p>:Class that defines a MLP layer. A MLP layer is built using the
		 * input vector and add an extra element (or neuron) to account for the intercept
		 * weight w0. The MLP layer is fully defined by its rank in the Neuron Network with
		 * input layer having id = 0 and the output layer having id = number of layers -1.</p>
		 * @param id id or rank of the MLP layer in the network
		 * @param len number of elements or neuron in the MLP layer
		 * @exception IllegalArgumentException if the class parameters are incorrect
		 * 
		 * @author Patrick Nicoloas
		 * @date May 6, 2014
		 * @project Scala for Machine Learning
		 */
import ScalaMl._
import MLP._
protected class MLPLayer(val id: Int, val len: Int) {
   require(id >= 0, "Create a MLP layer with incorrect id: " + id)
   require(len > 0, "Create a MLP layer with zero element")
   
   val output = new DblVector(len)
   val errors = new DblVector(len)
   output.update(0, 1.0)

   
   		/**
   		 * <p>Initialize the value of the input for this MLP layer.</p>
   		 * @param _input input vector for this layer. 
   		 * @exception IllegalArgumentException if the input vector is undefined
   		 */
   @inline
   def setInput(_input: DblVector): Unit = {
  	 require(_input != null, "Cannot initialize this MLP layer " + id  + " with undefined data")
  	 _input.copyToArray(output, 1)
   }


   		/**
   		 * <p>Compute the mean squares error of the neurons/elements of this MLP layer.</p>
   		 * @param labels target output value
   		 * @param objective function used to compute the error = target - output
   		 * @param gamma gamma or gain of the logistic or tanh function
   		 * 
   		 */
   def mse(labels: DblVector, objective: MLP.MLPObjective,  gamma: Double): Double = {
  	  require(labels != null, "Cannot compute the mean square error for a MLP layer with undefined labeled values")
  	  require(output.size == labels.size+1, "The size of the output " + output.size + " should be equal to target + 1" + labels.size+1)
  	  
	  var sumMSE = 0.0
	  output.drop(1).zipWithIndex.foreach(on => {
	  	 objective += (on._1, on._2)
  	  	 val err = labels(on._2) - on._1
	     errors.update(on._2+1, gamma*on._1* (1.0- on._1)*err)
	  	 sumMSE += err*err
	  })
	  sumMSE*0.5
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
		 * @date May 5, 2014
		 */
object MLPLayer {
  def apply(id: Int, len: Int): MLPLayer = new MLPLayer(id, len)
}



// -------------------------------------  EOF ------------------------------------------------
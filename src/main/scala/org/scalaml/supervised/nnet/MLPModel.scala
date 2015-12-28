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

import org.scalaml.core.Design.Model
import MLPModel._


		/**
		 * Model (synapse weights) for this multi-layer perceptron. A synapse is the pair
		 * of weight and weight derivative
		 * @param synapses Vector of matrices of synapses..
		 * 
		 * @author Patrick Nicolas
		 * @version 0.99.1
		 * @see Scala for Machine Learning Chapter 10 ''Artificial Neural Network'' / Multi-layer
		 * perceptron
		 */
@throws(classOf[IllegalArgumentException])
class MLPModel(val synapses: Vector[MLPConnSynapses]) extends Model {
	require(synapses.nonEmpty, "MLPModel synapses undefined" )
	
	final def getSynapses(n: Int): MLPConnSynapses = synapses(n)
	
	final def getTopology: Array[Int] = 
		Array[Int](synapses.head.head.length) ++ synapses.map( _.head.length)
	
		/**
		 * Write the content of this model (weights) into a file
		 * @return true if the model/weights were saved into file, false otherwise.
		 */
	override def >> : Boolean = write(s"$toString")
	
	override def toString: String = 
		synapses.map( _.map( _.mkString(",")).mkString("\n")).mkString("\n")
}


object MLPModel {
	type MLPSynapse = (Double, Double)
	type MLPConnSynapses = Array[Array[MLPSynapse]]
}

// ----------------------  EOF -----------------
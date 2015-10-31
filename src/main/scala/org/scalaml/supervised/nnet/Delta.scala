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
 * Version 0.99
 */
package org.scalaml.supervised.nnet

import org.scalaml.core.Types.ScalaMl._
import MLPModel._

		/**
		 * Class that defines the Delta error used in the MLP back propagation
		 * @constructor Create a Delta instance with an initial array of output errors, 
		 * the delta errors and the synapses the destination connection in the backpropagation and the 
		 * @param loss or error (expected value - predicted value) for each output variable
		 * @param delta Matrix of delta errors computed in the previous connection by the 
		 * backpropagation algorithm
		 * @param synapses Synapses (weights, delta weights) of the previous connection used in the
		 * backpropagation.
		 * 
		 * @author Patrick Nicolas
		 * @since 0.99 August 9, 201t
		 * @version 0.99
		 * @see Scala for Machine Learning Chapter 9 ''Artificial Neural Network'' / Multilayer 
		 * perceptron Model definition
		 * @see org.scalaml.supervised.nnet.MLPConnection
		 */
case class Delta(
		val loss: DblArray, 
		val delta: DblMatrix = Array.empty[DblArray],
		val synapses: MLPConnSynapses = Array.empty[Array[MLPSynapse]] ) {
  
	override def toString: String = {
		val losses = loss.mkString(",")
		
		if( delta.length > 0) {
			val deltas = delta.map( _.mkString(", ")).mkString("\n")
			s"Losses: $losses\ndelta\n$deltas" 
		}
		else s"Losses: $losses"
	} 
}

// ----------------------- EOF ----------------------------


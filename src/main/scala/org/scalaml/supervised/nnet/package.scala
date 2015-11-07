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
package org.scalaml.supervised

		/**
		 * This package object contains the classes implementing the multi-layer perceptron
		 * 
		 * - Typical configuration of the multi-layer perceptron as binomial, multinomial classifier
		 * as well as regression algorithm '''MLPConfig'''
		 * 
		 * - Definition of the objective or application of the multi-layer perceptron as binomial,
		 * multinomial or regression model '''MLPMode'''
		 * 
		 * - Definition of a layer (or neuron) in the multi-layer perceptron architecture'''MLPLayer'''
		 * 
		 * - Definition of a connection between consecutive layers in the multi-layer perceptron 
		 * architecture '''MLPConnection'''
		 * 
		 * - Model or topology with synapse weights in a multi-layer perceptron '''MLPNetwork'''
		 * 
		 * - Implementation of the multi-layer perceptron learning algorithms '''MLP'''
		 * @see Scala for Machine Learning Chapter 9 ''Artificial Neural Network'' / Multi-layer 
		 * perceptron
		 */
package object nnet { }
// ---------------------------------------  EOF -----------------------------------------
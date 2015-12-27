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
package org.scalaml.supervised

		/**
		 * This package object contains the classes implementing the generative models associated
		 * with the Hidden Markov algorithms
		 * 
		 * - Alpha or forward pass as the probability of being in state S(i) given a sequence of 
		 * observations {0, 1,   t} '''Alpha'''
		 * - Beta or backward pass as the probability of being in state S(i) given the observations 
		 * {t+1, t+2, ... T-1} '''Beta'''
		 * - Typical configuration of a Hidden Markov model '''HMMConfig'''
		 * - Definition of the lambda model containing the state transition probabilities, emission
		 * probabilities and initial state probabilities matrices for the Hidden Markov model 
		 * '''HMMModel'''
		 * - Baum-Welch estimator (Expectation-Maximization) for training a Hidden Markov model 
		 * '''BaumWelchEM'''
		 * - Implementation of the Viterbi to extract the best sequence of hidden states in a HMM 
		 * given a lambda model and a sequence of observations '''ViterbiPath'''
		 * - Implementation of the Hidden Markov model &#39;&#39;&#39;HMM&#39;&#39;&#39;
		 * 
		 * @see Scala for Machine Learning Chapter 7 ''Sequential Data Models'' / Hidden Markov model
		 */
package object hmm { }

// ---------------------------------------  EOF -----------------------------------------
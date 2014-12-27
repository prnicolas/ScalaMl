/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 * 
 * Version 0.98
 */
package org.scalaml.supervised

		/**
		 * This package object contains the classes implementing the generative models associated
		 * with the Hidden Markov algorithms<br>
		 * - Alpha or forward pass as the probability of being in state S(i) given a sequence of 
		 * observations {0, 1,   t} <b>Alpha</b><br>
		 * - Beta or backward pass as the probability of being in state S(i) given the observations 
		 * {t+1, t+2, ... T-1} <b>Beta</b><br>
		 * - Typical configuration of a Hidden Markov model <b>HMMConfig</b><br>
		 * - Definition of the lambda model containing the state transition probabilities, emission
		 * probabilities and initial state probabilities matrices for the Hidden Markov model 
		 * <b>HMMLambda</b>
		 * - Baum-Welch estimator (Expectation-Maximization) for training a Hidden Markov model 
		 * <b>BaumWelchEM</b><br>
		 * - Implementation of the Viterbi to extract the best sequence of hidden states in a HMM 
		 * given a lambda model and a sequence of observations <b>ViterbiPath</b><br>
		 * - Implementation of the Hidden Markov model <b>HMM</b><br>
		 * @note Scala for Machine Learning Chapter 7 Sequential data models / Hidden Markov model
		 */
package object hmm { }
// ---------------------------------------  EOF -----------------------------------------
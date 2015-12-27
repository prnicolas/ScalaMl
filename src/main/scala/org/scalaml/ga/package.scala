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
package org.scalaml

		/**
		 * This package object defines the classes that support genetic algorithms as
		 * classifiers and optimizer introduced in Chapter 10: Genetic Algorithms
		 * 
		 * - Genetic chromosome defined as a list of Gene with a symbolic representation of list of
		 * predicates (variable operator value) or rule IF( value operator value) THEN ..'''Chromosome
		 * '''
		 * 
		 * - Genetic Gene defined in binary format with a symbolic representation of predicates
		 *  '''Gene'''
		 *  
		 * - Chromosome pool or population managed by genetic operator such as selection, mutation
		 * and cross-over '''Population'''
		 * 
		 * - Reproduction cycle (selection, cross-over, mutation with exit conditions 
		 * '''Reproduction'''
		 * 
		 * - Genetic Algorithm solver '''GASolver'''<br>
		 * @see Scala for Machine Learning - Chapter 10 Genetic algorithm
		 */
package object ga { }
// ---------------------------------------  EOF -----------------------------------------
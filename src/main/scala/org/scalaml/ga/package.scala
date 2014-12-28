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
package org.scalaml

		/**
		 * This package object defines the classes that support genetic algorithms as
		 * classifiers and optimizer introduced in Chapter 10: Genetic Algorithms<br>
		 * - Genetic chromosome defined as a list of Gene with a symbolic representation of list of
		 * predicates (variable operator value) or rule IF( value operator value) THEN ..<b>Chromosome</b><br>
		 * - Genetic Gene defined in binary format with a symbolic representation of predicates
		 *  <b>Gene</b><br>
		 * - Chromosome pool or population managed by genetic operator such as selection, mutation
		 * and cross-over <b>Population</b><br>
		 * - Reproduction cycle (selection, cross-over, mutation with exit conditions 
		 * <b>Reproduction</b><br>
		 * - Genetic Algorithm solver <b>GASolver</b><br>
		 * @note Scala for Machine Learning - Chapter 10 Genetic algorithm
		 */
package object ga { }
// ---------------------------------------  EOF -----------------------------------------
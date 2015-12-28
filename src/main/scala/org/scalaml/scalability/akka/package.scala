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
package org.scalaml.scalability

		/**
		 * This package object encapsulates some basic applications of the Akka framework. The 
		 * different configurations to illustrate basic usage of Akka actors and futures are:
		 * 
		 * - Generic controller for distributed computation '''Controller'''
		 * 
		 * - Class to split or partition time series for concurrent processing '''Partitioner'''
		 * 
		 * - Implementation of basic Master-workers pattern '''Worker''' and '''Master'''
		 * 
		 * - Master-worker design with a supervising router '''MasteWithRouter'''
		 * 
		 * - Implementation of futures with blocking of the client actor to distribute the 
		 * computation of the Discrete Fourier transform'''TransformFutures'''
		 * 
		 * - Implementation of futures with callback to the client actor to distribute the 
		 *  computation of the Discrete Fourier transform'''TransformFuturesClbck'''
		 *  
		 * @see Scala for Machine Learning Chapter 12 Scalable frameworks
		 * @note The name of the Akka actor routing protocol has been changed in version 2.3.
		 * @note The version of Akka library has to be compatible with the version of Apache Spark.
		 */
package object akka { }
// ---------------------------------------  EOF -----------------------------------------
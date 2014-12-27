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
package org.scalaml.scalability

		/**
		 * This package object encapsulates some basic applications of the Akka framework. The 
		 * different configurations to illustrate basic usage of Akka actors and futures are:<br>
		 * - Generic controller for distributed computation <b>Controller</b><br>
		 * - Class to split or partition time series for concurrent processing <b>Partitioner</b><br>
		 * - Implementation of basic Master-workers pattern <b>Worker</b> and <b>Master</b><br>
		 * - Master-worker design with a supervising router <b>MasteWithRouter</b><br>
		 * - Implementation of futures with blocking of the client actor to distribute the 
		 * computation of the Discrete Fourier transform<b>TransformFutures</b><br>
		 * - Implementation of futures with callback to the client actor to distribute the 
		 *  computation of the Discrete Fourier transform<b>TransformFuturesClbck</b><br>
		 * @note Scala for Machine Learning Chapter 12 Scalable frameworks
		 * @note The name of the Akka actor routing protocol has been changed in version 2.3.
		 * @note The version of Akka library has to be compatible with the version of Apache Spark.
		 */
package object akka { }
// ---------------------------------------  EOF -----------------------------------------
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
package org.scalaml.app.chap12

import org.scalaml.app.ScalaMlTest


		/**
		 * Test driver for the techniques described in the Chapter 12 Scalable frameworks
		 * {{{
		 * 	 Scala parallel collections
		 * 	 Akka actors Master-worker
		 *   Akka futures
		 *   Apache Spark MLlib-K-mean
		 * }}}
		 * @see org.scalaml.app.ScalaMlTest
		 * @author Patrick Nicolas
		 * @since 0.98.1 May 28, 2014
		 * @see Scala for Machine Learning Chapter 12 Scalable frameworks
		 */
final class Chap12 extends ScalaMlTest  {
		/**
		 * Name of the chapter the tests are related to
		 */
	val chapter: String = "Chapter 12"
		/**
		 * Maximum duration allowed for the execution of the evaluation
		 */
	val maxExecutionTime: Int = 7
	
	test(s"$chapter Scala parallel collections") {
		evaluate(ParBenchmarkEval)
	}
	test(s"$chapter Akka actors Master-worker") {
		evaluate(ActorsManagerEval)
	}
	
	test(s"$chapter Akka futures") {
		evaluate(TransformFuturesEval)
	}

//	test(s"$chapter Apache Spark MLlib-K-means") {
//		evaluate(SparkKMeansEval)
 //	}
}


// ---------------------------------  EOF -------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.app.chap12

import org.scalaml.app.ScalaMlTest



final class Chap12 extends ScalaMlTest  {
	val chapter: String = "Chap 12"
	val maxExecutionTime: Int = 10000
  	 
	test(s"$chapter Scala parallel collections") {
	   evaluate(ParBenchmarkEval)
	}
	test(s"$chapter Akka actors evaluation") {
	   evaluate(ActorsManagerEval)
	}
	
	test(s"$chapter Akka futures evaluation") {
		evaluate(TransformFuturesEval)
	}

	test(s"$chapter Spark K-means evaluation") {
		evaluate(SparkKMeansEval)
	}
}


// ---------------------------------  EOF -------------------------
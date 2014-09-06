/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.92
 */
package org.scalaml.app.chap7

import org.scalaml.util.Matrix
import org.scalaml.supervised.hmm.{HMM, HMMForm, HMMLambda}
import org.scalaml.supervised.crf.{CrfConfig,  CrfSeqDelimiter, Crf}
import java.io.IOException



		/**
		 * Singleton for the evaluation of Conditional Random Fields
		 * presented in chapter 7
		 * 
		 * @author Patrick Nicolas
		 * @since March 28, 2014
		 * @note Scala for Machine Learning
		 */
object CrfEval {
  def run: Unit = {
    Console.println("Evaluation of Conditional Random Fields")
    
	val lambda = 0.5
	val nLabels = 9
	val config = CrfConfig(0.5, 100, lambda, 0.01)
	val delimiters = CrfSeqDelimiter(",\t/ -():.;'?#`&_", "//", "\n")
	    
	try {
	   Crf(nLabels, config, delimiters, "resources/data/chap7/rating").weights match {
	  	 case Some(weights) => weights.foreach( println )
	  	 case None =>  println("Count not train the CRF model")
	   }
	 }
     catch {
		case e: IllegalArgumentException => Console.println(e.toString)
		case e: IOException => Console.println(e.toString)
	 }
  }
}

// --------------------------------  EOF -------------------------------
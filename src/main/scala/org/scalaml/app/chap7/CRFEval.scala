/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
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
		 * @date March 28, 2014
		 * @project Scala for Machine Learning
		 */
object CrfEval {
  def run: Unit = {
    Console.println("Evaluation of Conditional Random Fields")
    
	val lambda = 0.5
	val nLabels = 9
	val config = CrfConfig(0.7, 100, lambda, 0.01)
	val delimiters = CrfSeqDelimiter(",\t/ -():.;'?#`&_", "//", "\n")
	    
	try {
	   Crf(nLabels, config, delimiters, "resources/data/chap7/rating").weights match {
	  	 case Some(model) => model.weights.foreach( println )
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
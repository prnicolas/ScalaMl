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
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display



		/**
		 * Singleton for the evaluation of Conditional Random Fields
		 * presented in chapter 7
		 * 
		 * @author Patrick Nicolas
		 * @since March 28, 2014
		 * @note Scala for Machine Learning
		 */
object CrfEval {
  final val LAMBDA = 0.5
  final val NLABELS = 9
  final val MAX_ITERS = 100
  final val W0 = 0.7
  final val EPS = 1e-3
  final val PATH = "resources/data/chap7/rating"
  
  private val logger = Logger.getLogger("CrfEval")
  
  def run: Unit = {
    Display.show("Evaluation of Conditional Random Fields", logger)
    

	val state = CrfConfig(W0 , MAX_ITERS, LAMBDA, EPS)
	val delimiters = CrfSeqDelimiter(",\t/ -():.;'?#`&_", "//", "\n")
	    
	Try {
	   Crf(NLABELS, state, delimiters, PATH).weights match {
	  	 case Some(weights) => weights
	  	 case None => throw new IllegalStateException("Count not train the CRF model")
	   }
	 } match {
		case Success(weights) => Display.show("CrfEval " + weights, logger)
		case Failure(e) => Display.error("CrfEval ", logger, e)
	 }
  }
}

// --------------------------------  EOF -------------------------------
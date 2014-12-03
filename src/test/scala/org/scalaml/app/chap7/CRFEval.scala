/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96d
 */
package org.scalaml.app.chap7

import org.scalaml.util.Matrix
import org.scalaml.supervised.hmm.{HMM, HMMForm, HMMLambda}
import org.scalaml.supervised.crf.{CrfConfig,  CrfSeqDelimiter, Crf}
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display
import org.scalaml.app.Eval
import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.core.Types.ScalaMl


		/**
		 * Singleton for the evaluation of Conditional Random Fields
		 * presented in chapter 7
		 * 
		 * @author Patrick Nicolas
		 * @since March 28, 2014
		 * @note Scala for Machine Learning
		 */
object CrfEval extends Eval {
	val name: String = "CrfEval"
	val maxExecutionTime: Int = 12000
	
	private val LAMBDA = 0.5
	private val NLABELS = 9
	private val MAX_ITERS = 100
	private val W0 = 0.7
	private val EPS = 1e-3
	private val PATH = "resources/data/chap7/rating"
  
	private val logger = Logger.getLogger(name)

		/**
		 * <p>Execution of the scalatest for <b>Crf</b> class.
		 * This method is invoked by the  actor-based test framework function, ScalaMlTest.evaluate</p>
		 * @param args array of arguments used in the test
		 * @return -1 in case error a positive or null value if the test succeeds. 
		 */
	def run(args: Array[String]): Int = {
		Display.show(s"\n\n *****  test#${Eval.testCount} $name Conditional Random Fields", logger)
    
		val state = CrfConfig(W0 , MAX_ITERS, LAMBDA, EPS)
		val delimiters = new CrfSeqDelimiter(",\t/ -():.;'?#`&_", "//", "\n")
	    
		Try {
			val crf = Crf(NLABELS, state, delimiters, PATH)
			crf.weights match {
				case Some(w) => {
					Display.show(s"$name weights (lambdas) for conditional random fields", logger)
					Display.show(s"${ScalaMl.toString(w, "", true)}", logger)
				}
				case None => throw new IllegalStateException(s"$name Could not train the CRF model")
			}
		} 
		match {
			case Success(res) => 1
			case Failure(e) => Display.error(s"$name CRF modeling failed", logger, e)
		}
  }
}


// --------------------------------  EOF -------------------------------
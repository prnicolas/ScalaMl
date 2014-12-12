/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.2
 */
package org.scalaml.app.chap7

import org.scalaml.supervised.crf.{CrfConfig,  CrfSeqDelimiter, Crf}
import org.scalaml.util.{DisplayUtils, Matrix, FormatUtils}
import org.scalaml.app.Eval
import org.scalaml.core.Types.ScalaMl.DblVector


		/**
		 * <p><b>Purpose:</b>Singleton for the evaluation of Conditional Random Fields
		 * for sentiment analysis.</p>
		 * @author Patrick Nicolas
		 * @note Scala for Machine Learning  Chapter 7 Sequential data models/Conditional Random Fields.
		 */
object CrfEval extends Eval {
	import scala.util.{Try, Success, Failure}
	import org.apache.log4j.Logger
	
		/**
		 * Name of the evaluation 
		 */
	val name: String = "CrfEval"
		/**
		 * Maximum duration allowed for the execution of the evaluation
		 */
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
		DisplayUtils.show(s"$header Conditional Random Fields", logger)
    
		val state = CrfConfig(W0 , MAX_ITERS, LAMBDA, EPS)
		val delimiters = new CrfSeqDelimiter(",\t/ -():.;'?#`&_", "//", "\n")
	    
		Try {
			val crf = Crf(NLABELS, state, delimiters, PATH)
			crf.weights match {
				case Some(w) => {
					DisplayUtils.show(s"$name weights (lambdas) for conditional random fields", logger)
					DisplayUtils.show(s"${FormatUtils.format(w, "", FormatUtils.ShortFormat)}", logger)
				}
				case None => throw new IllegalStateException(s"$name Could not train the CRF model")
			}
		} 
		match {
			case Success(res) => 1
			case Failure(e) => DisplayUtils.error(s"$name CRF modeling failed", logger, e)
		}
  }
}


// --------------------------------  EOF -------------------------------
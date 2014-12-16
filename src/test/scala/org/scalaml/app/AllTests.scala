/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.3
 */
package org.scalaml.app

import org.apache.log4j.Logger
import org.scalaml.app.core._
import org.scalaml.app.chap1._
import org.scalaml.app.chap2._
import org.scalaml.app.chap3._
import org.scalaml.app.chap4._
import org.scalaml.app.chap5._
import org.scalaml.app.chap6._
import org.scalaml.app.chap7._
import org.scalaml.app.chap8._
import org.scalaml.app.chap9._
import org.scalaml.app.chap10._
import org.scalaml.app.chap11._
import org.scalaml.app.chap12._
import org.scalaml.util.DisplayUtils

		/**
		 * <p>Singleton that executes all the test clases in Scala for Machine Learning.<br>
		 * The tests are triggered from the Simple Build Tool (SBT) and Scalatest using the
		 * command line <i>sbt test:run</i><br>
		 * Each test is implemented as an actor that terminates when either the test completes
		 * or the time out is exceeded.</p>
		 */
protected object AllTests extends ScalaMlTest {
	val chapter: String = "All tests"

	private val logger = Logger.getLogger("AllTests")
	
		/**
		 * Method to execute all the tests in Scala for Machine Learning
		 * Following the order of the chapters.
		 */
	def test: Unit = {
			// Core
		run(StatsEval)
		run(XTSeriesEval)
		run(MatrixEval)
		
			// Chapter 1
		run(LogBinRegressionEval)
		run(PlotterEval)
		
			// Chapter 2
		run(BiasVarianceEval)
		run(WorkflowEval)
		
			//Chapter 3
		run(MovingAveragesEval, Array[String]("BAC", "60")) 
		
		run(DFTEval)
		run(DFTEval, Array[String]("BAC"))
		run(DKalmanEval, Array[String]("BAC"))


			// Chapter 4
		val input = Array[String]("2", "3", "4", "7", "9", "10", "13", "15")
		run(KMeansEval, input)
		run(EMEval, Array[String]("2", "40"))
		run(EMEval, Array[String]("3", "25"))
		run(EMEval, Array[String]("4", "15"))
		run(PCAEval)

			// Chapter 5
		val TRAIN_VALIDATION_RATIO = "0.8"
		run(BinomialBayesEval, Array[String]("IBM", TRAIN_VALIDATION_RATIO, "8"))
		run(BinomialBayesEval, Array[String]("NEM", TRAIN_VALIDATION_RATIO, "4"))
		run(BinomialBayesEval, Array[String]("NEM", TRAIN_VALIDATION_RATIO, "12"))
		run(BinomialBayesEval, Array[String]("NEM", TRAIN_VALIDATION_RATIO, "36"))
		run(BinomialBayesEval, Array[String]("GE", TRAIN_VALIDATION_RATIO, "4"))
		run(BinomialBayesEval, Array[String]("GE", TRAIN_VALIDATION_RATIO, "12"))
		run(BinomialBayesEval, Array[String]("GE", TRAIN_VALIDATION_RATIO, "36"))
		run(BinomialBayesEval, Array[String]("BAC", TRAIN_VALIDATION_RATIO, "4"))
		run(BinomialBayesEval, Array[String]("BAC", TRAIN_VALIDATION_RATIO, "12"))
		run(BinomialBayesEval, Array[String]("BAC", TRAIN_VALIDATION_RATIO, "36"))
		run(TextBayesEval)
	
		run(FunctionClassificationEval)
		
			// Chapter 6
		run(SingleLinearRegressionEval)
		run(RidgeRegressionEval)
		run(MultiLinearRegressionEval, Array[String]("trend"))
		run(MultiLinearRegressionEval, Array[String]("filter"))

		run(LogisticRegressionEval)

			// Chapter 7
		run(HMMEval, Array[String]("evaluation"))
		run(HMMEval, Array[String]("training"))
		run(CrfEval)
		
			// Chapter 8
		run(SVCKernelEval)
		run(SVCMarginEval)
		run(SVCEval)
		run(SVCOutliersEval)
		run(SVREval)	
		
			// Chapter 9
	  run(MLPConfigEval, Array[String]("eta"))
		run(MLPConfigEval, Array[String]("alpha"))
		run(MLPValidation)
		run(BinaryMLPEval)
		run(MLPEval)
		
			// Chapter 10
		run(GAEval)
		
			// Chapter 11
		run(QLearningEval)
		
			// Chapter 12
		run(ParBenchmarkEval, Array[String]("array"))
		run(ParBenchmarkEval, Array[String]("map"))
		run(ActorsManagerEval, Array[String]("norouter"))
		run(ActorsManagerEval, Array[String]("router"))
		run(TransformFuturesEval)
		run(SparkKMeansEval)
	}
	
	
			/**
			 * Wrapper or helper method to execute the 
			 */
	private def run(eval: Eval, args: Array[String] = Array.empty): Unit = {
		import akka.actor.{ActorSystem, Actor, Props, ActorRef}
		import scala.util.{Try, Success, Failure}
		
		var completed = false
	  		
			// Anonymous Akka actor that wraps the execution of the scala test.
		val worker = TestContext.actorSystem.actorOf(Props(new Actor {
			def receive = { 
					// Launch the execution of a test within 
					// the event handler of the worker actor.
				case msg: String => {
					completed = evaluate(eval, args)
					context.stop(self)
				}
				case _ => { } // Ignore
			}
		}))
		var errorMsg = "failed"
		
			// The main thread blocks until either the maxExecutionTime is reached
			// of the computation status has been updated...
		Try {
			worker ! "Start"
			val startTime = System.currentTimeMillis
			while( !completed ) {
				Thread.sleep(200)
				
				// Exit if time out 'maxExecutionTime" is exceeded
				if(System.currentTimeMillis - startTime > eval.maxExecutionTime) {
					completed = true
					errorMsg = s"time out after $eval.maxExecutionTime msecs."
				}
			}
			DisplayUtils.show(s"End ${eval.name}", logger)
			completed
		} 
	  	match {
			case Success(n) => { } // No message needed
			case Failure(e) => DisplayUtils.error(s"$chapter failed ", logger, e)
		}
	}
}


		/**
		 * <p>Driver called by simple build tool (SBT) as test:run
		 * @author Patrick Nicolas
		 */
object AllTestsApp extends App  {
	TestContext.init
	AllTests.test
	TestContext.shutdownAll
	println("end")
}


// ------------------------------------  EOF ----------------------------------------------------
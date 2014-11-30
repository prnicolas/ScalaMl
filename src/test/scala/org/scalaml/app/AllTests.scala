/**
 * Copyright 2013, 2014, 2015  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96
 */
package org.scalaml.app

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
import akka.actor.{ActorSystem, Actor, Props, ActorRef}
import scala.util.{Try, Success, Failure}
import org.apache.log4j.Logger
import org.scalaml.util.Display



		/**
		 * <p>Singleton that executes all the test clases in Scala for Machine Learning.<br>
		 * The tests are triggered from the Simple Build Tool (SBT) and Scalatest using the
		 * command line <i>sbt test:run</i></p>
		 * @param chapter Name of the chapter for cross-reference
		 */
protected object AllTests extends ScalaMlTest {
	val chapter: String = "All-Tests"
	
	implicit val actorSystem = ActorSystem("system")
	private val logger = Logger.getLogger("AllTests")
	
		/**
		 * Method to execute all the tests in Scala for Machine Learning
		 * Following the order of the chapters.
		 */
	def test: Unit = {
			// Chapter 6
		run(LogBinRegressionEval)
		run(PlotterEval)
		
			// Chapter 2
		run(BiasVarianceEval)
		run(WorkflowEval)
			
			//Chapter 3
		run(MovingAveragesEval, Array[String]("BAC", "10")) 
		run(DFTEval)
		run(DFTEval, Array[String]("BAC"))
		run(new DKalmanEval, Array[String]("BAC"))
	
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
		
			// Chapter 6
		run(FunctionClassificationEval)
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
		run(SVCEval)
		run(SVCKernelEval)
		run(SVCOutliersEval)
		run(SVCMarginEval)
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
		run(ParBenchmarkEval)
		run(ActorsManagerEval)
		run(TransformFuturesEval)
		run(SparkKMeansEval)
		
		Display.show(s"$chapter exit", logger)
		actorSystem.shutdown
	}
	
	private def run(eval: Eval, args: Array[String] = Array.empty) {
		var completed = false
	  		// Anonymous Akka actor that wraps the execution of the scala test.
		val worker = actorSystem.actorOf(Props(new Actor {
			def receive = { 
				case msg: String => {
					completed = evaluate(eval, args)
					context.stop(self)
				}
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
			completed
		} match {
			case Success(n) => Display.show(s"$chapter done", logger)
			case Failure(e) => Display.error(s"$chapter failed ", logger, e)
		}
	}
}



object AllTestsApp extends App  {
	AllTests.test
}


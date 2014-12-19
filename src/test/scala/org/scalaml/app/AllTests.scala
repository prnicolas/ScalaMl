/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.97.3
 */
package org.scalaml.app

import scala.annotation.switch
import scala.util.Properties

import org.scalatest.concurrent._
import org.scalatest.time.{Span, Seconds, Millis}
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
	  
	private val CONFIGURATION = "Recommended SBT/JVM configuration:\n-Xmx4096 (or higher)\n" +
			" -XX:MaxPermSize=512m (or higher)\n -XX:ReservedCodeCacheSize=256m (or higher)\n" +
			s"Context:\nUser:${Properties.userName}, OS:${Properties.osName}"

	private val logger = Logger.getLogger("AllTests")
	
		/**
		 * Method to execute all the tests in Scala for Machine Learning
		 * Following the order of the chapters.
		 */
	def run: Unit = {
		header
			// Core
		evaluate(StatsEval)
		
		evaluate(XTSeriesEval)
		evaluate(MatrixEval)

			// Chapter 1
		evaluate(LogBinRegressionEval)
		evaluate(PlotterEval)
		
			// Chapter 2
		evaluate(BiasVarianceEval)
		evaluate(WorkflowEval)
		
			//Chapter 3
		evaluate(MovingAveragesEval, Array[String]("BAC", "60")) 
		evaluate(DFTEval)
		evaluate(DFTEval, Array[String]("BAC"))
		evaluate(DKalmanEval, Array[String]("BAC"))


			// Chapter 4
		val input = Array[String]("2", "3", "4", "7", "9", "10", "13", "15")
		evaluate(KMeansEval, input)
		evaluate(EMEval, Array[String]("2", "40"))
		evaluate(EMEval, Array[String]("3", "25"))
		evaluate(EMEval, Array[String]("4", "15"))
		
		evaluate(PCAEval)

			// Chapter 5
		val TRAIN_VALIDATION_RATIO = "0.8"
		evaluate(BinomialBayesEval, Array[String]("IBM", TRAIN_VALIDATION_RATIO, "8"))
		evaluate(BinomialBayesEval, Array[String]("NEM", TRAIN_VALIDATION_RATIO, "4"))
		evaluate(BinomialBayesEval, Array[String]("NEM", TRAIN_VALIDATION_RATIO, "12"))
		evaluate(BinomialBayesEval, Array[String]("NEM", TRAIN_VALIDATION_RATIO, "36"))
		evaluate(BinomialBayesEval, Array[String]("GE", TRAIN_VALIDATION_RATIO, "4"))
		evaluate(BinomialBayesEval, Array[String]("GE", TRAIN_VALIDATION_RATIO, "12"))
		evaluate(BinomialBayesEval, Array[String]("GE", TRAIN_VALIDATION_RATIO, "36"))
		evaluate(BinomialBayesEval, Array[String]("BAC", TRAIN_VALIDATION_RATIO, "4"))
		evaluate(BinomialBayesEval, Array[String]("BAC", TRAIN_VALIDATION_RATIO, "12"))
		evaluate(BinomialBayesEval, Array[String]("BAC", TRAIN_VALIDATION_RATIO, "36"))
		evaluate(TextBayesEval)
	
		evaluate(FunctionClassificationEval)
		
			// Chapter 6
		evaluate(SingleLinearRegressionEval)
		evaluate(RidgeRegressionEval)
		evaluate(MultiLinearRegressionEval, Array[String]("trend"))
		evaluate(MultiLinearRegressionEval, Array[String]("filter"))

		evaluate(LogisticRegressionEval)

			// Chapter 7
		evaluate(HMMEval, Array[String]("evaluation"))
		evaluate(HMMEval, Array[String]("training"))
		evaluate(CrfEval)
		
			// Chapter 8
		evaluate(SVCKernelEval)
		evaluate(SVCMarginEval)
		evaluate(SVCEval)
		evaluate(SVCOutliersEval)
		evaluate(SVREval)	
		
			// Chapter 9
	  evaluate(MLPConfigEval, Array[String]("eta"))
		evaluate(MLPConfigEval, Array[String]("alpha"))
		evaluate(MLPValidation)
		evaluate(BinaryMLPEval)
		evaluate(MLPEval)
		
			// Chapter 10
		evaluate(GAEval)
		
			// Chapter 11
		evaluate(QLearningEval)
		
			// Chapter 12
		evaluate(ParBenchmarkEval, Array[String]("array"))
		evaluate(ParBenchmarkEval, Array[String]("map"))
		evaluate(ActorsManagerEval, Array[String]("norouter"))
		evaluate(ActorsManagerEval, Array[String]("router"))
		evaluate(TransformFuturesEval)
		evaluate(SparkKMeansEval)
	}
		
		/**
		 * Method to validate the version of Scala and Java JDK used.
		 */
	private def header: Unit = {
		DisplayUtils.show(CONFIGURATION, logger)
		if( !Properties.isWin && !Properties.isMac)
			DisplayUtils.show("The library has not be tested for this Operating System", logger)
			
		DisplayUtils.show(s"Java version: ${Properties.javaVersion}", logger)
		if(!Properties.isJavaAtLeast("1.7"))
			DisplayUtils.show("Incompatible version of Java, should be 1.7 or later", logger)
			
		val scalaVersion = Properties.versionNumberString
		
		DisplayUtils.show(s"Scala version: $scalaVersion", logger)
		(scalaVersion.charAt(2): @switch) match {
			case '9' => DisplayUtils.show("Scala version should be 2.10.2 or higher", logger)
			case '1' => {
				(scalaVersion.charAt(3): @switch) match {
					case '0' => DisplayUtils.show("Compatible Akka version should be 2.2.4 or lower", logger)
					case '1' => DisplayUtils.show("Compatible Akka version should be 2.3.4 or higher", logger)
				}
			}
			case _ => DisplayUtils.show("Could not initialize", logger)
		}
		count = 0
	}
	
	var count: Int = _	
	def testCount: String = { count += 1;  String.valueOf(count) }
}


		/**
		 * <p>Driver called by simple build tool (SBT) as test:run
		 * @author Patrick Nicolas
		 */
object AllTestsApp extends App { AllTests.run }


// ------------------------------------  EOF ----------------------------------------------------
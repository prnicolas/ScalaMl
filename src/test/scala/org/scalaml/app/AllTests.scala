/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.98
 */
package org.scalaml.app

	// Scala standard library
import scala.annotation.switch
import scala.util.Properties
	// Third party frameworks
import org.scalatest.concurrent._
import org.scalatest.time.{Span, Seconds, Millis}
import org.apache.log4j.Logger
	// ScalaMl classes
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
	  
	private val CONFIGURATION = "Recommended SBT/JVM configuration:\n -Xmx4096 (or higher)\n" +
			" -XX:MaxPermSize=512m (or higher)\n -XX:ReservedCodeCacheSize=256m (or higher)\n" +
			s"Context:\nUser:${Properties.userName}, OS:${Properties.osName}"

	private val logger = Logger.getLogger("AllTests")
	
		/**
		 * Method to execute all the tests in Scala for Machine Learning
		 * Following the order of the chapters. See individual class xxxEval 
		 * for description and purpose of the test
		 */
  def run: Unit = {
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
		evaluate(MLPEval, Array[String]("4"))
		evaluate(MLPEval, Array[String]("4", "4"))
		evaluate(MLPEval, Array[String]("7", "7"))
		evaluate(MLPEval, Array[String]("5", "5", "5"))
				
			// Chapter 10
		evaluate(GAEval)

			// Chapter 11
		evaluate(QLearningEval, Array[String]("maxReward", "2", "4"))
		evaluate(QLearningEval, Array[String]("maxReward", "8", "4"))
		evaluate(QLearningEval, Array[String]("maxReward", "8", "5"))
		evaluate(QLearningEval, Array[String]("random"))
		
			// Chapter 12
		evaluate(ParBenchmarkEval, Array[String]("array"))
		evaluate(ParBenchmarkEval, Array[String]("map"))
		evaluate(ActorsManagerEval, Array[String]("norouter"))
		evaluate(ActorsManagerEval, Array[String]("router"))
		evaluate(TransformFuturesEval)
		evaluate(SparkKMeansEval)
	}
		
		/**
		 * Method to display the current settings of configuration and
		 * validate the version of Scala and Java JDK used.
		 */
	def header(args: Array[String]): Unit = {
		val buf = new StringBuilder("\nCommand line configuration for output:")
		args.foreach(arg => buf.append(s" $arg"))
		buf.append("\n")
			// Display configuration and settings information  regarding OS
		buf.append(CONFIGURATION)
		if( !Properties.isWin && !Properties.isMac)
			buf.append("The library has not be tested for this Operating System")
	
			// Correct version of Java
		buf.append(s" Java version: ${Properties.javaVersion}\n")
		if(!Properties.isJavaAtLeast("1.7"))
			buf.append("Incompatible version of Java, should be 1.7 or later\n")
			
			// Correct version of Scala and AKka
		val scalaVersion = Properties.versionNumberString
		buf.append(s"Scala version: $scalaVersion\n")
		(scalaVersion.charAt(2): @switch) match {
			case '9' => buf.append("Scala version should be 2.10.2 or higher")
			case '1' => {
				(scalaVersion.charAt(3): @switch) match {
					case '0' => buf.append("Compatible Akka version should be 2.2.4 or lower")
					case '1' => buf.append("Compatible Akka version should be 2.3.4 or higher")
				}
			}
			case _ => buf.append("Could not initialize")
		}
		DisplayUtils.show(buf.toString, logger)
		count = 0
	}
	var count: Int = _	
	def testCount: String = { count += 1;  String.valueOf(count) }
}


		/**
		 * <p>Driver called by simple build tool (SBT) as test:run
		 * @author Patrick Nicolas
		 */
object AllTestsApp extends App {
	if( !args.isEmpty ) {
		DisplayUtils.init(args)
		AllTests.header(args)
	}	
	else
		AllTests.header(Array[String]("console", "chart"))
	AllTests.run 
}


// ------------------------------------  EOF ----------------------------------------------------
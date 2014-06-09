/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap1

import scala.io.Source
import java.awt.Color
import org.scalaml.plots._
import org.scalaml.stats.Stats
import org.scalaml.trading.{Signal, PriceVolume}
import org.scalaml.core.Types
import Signal._
import Types.ScalaMl._
import org.scalaml.supervised.regression.LogBinRegression



		/**
		 * <p>Test driver for the techniques described in this chapter<br>
		 * <ul>
		 *   <li>JFreeChart plots</li>
		 *   <li>Logistic Binary classifier</li>
		 * </ul></p>
		 * @author Patrick Nicolas
		 * @data December 11, 2013
		 * @project Scala for Machine Learning.
		 */
object Chap1 extends App {
	PlotterEval.run(args)
	LogBinRegressionEval.run(args)
}

// --------------------  EOF --------------------------------------
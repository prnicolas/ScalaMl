/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the only purpose of illustrating the 
 * concepts and algorithms presented in Scala for Machine Learning.
 */
package org.scalaml.app.chap2

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import org.scalaml.stats.{Stats, BiasVarianceEmulator}
import org.scalaml.workflow._
import scala.util.Random
import org.scalaml.core.Types.ScalaMl._
import org.scalaml.plots.LightPlotTheme



		/**
		 * <p>Test driver for the techniques described in this chapter<br>
		 * <ul>
		 *   <li>JDependency injection base workflow</li>
		 *   <li>Bias-Variance decomposition</li>
		 * </ul></p>
		 * 
		 * @author Patrick Nicolas
		 * @data February 1, 201t
		 * @project Scala for Machine Learning.
		 */
object Chap2 extends App {
	WorkflowEval.run(args)
	BiasVarianceEval.run(args)
}

     
// -------------------------------------  EOF -----------------------------------------
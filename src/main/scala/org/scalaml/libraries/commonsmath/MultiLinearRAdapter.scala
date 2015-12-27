/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License") you may not use this file 
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * 
 * Version 0.99.1
 */
package org.scalaml.libraries.commonsmath

import scala.language.implicitConversions
import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression

import org.scalaml.core.Types.ScalaMl._




/**
		 * Implicit conversion from internal primitive types DblArray and DblMatrix to Apache 
		 * Commons Math types.
		 * @author Patrick Nicolas
		 * @since January 23, 2014 0.98
		 * @version 0.98.1
		 * @see Scala for Machine Learning Chapter 3 Data pre-processing/Time series
		 */

	final class MultiLinearRAdapter extends OLSMultipleLinearRegression {
	  
		final def createModel(y: DblVector, x: XVSeries[Double]): Unit = 
				super.newSampleData(y.toArray, x.toArray)
		final def weights: DblArray = estimateRegressionParameters
		final def rss: Double = calculateResidualSumOfSquares
	}
	
// ---------------------------  EOF -----------------------------
/**
 * Copyright 2013, 2014  by Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning" ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.95c
 */
package org.scalaml.supervised.regression


import org.scalaml.core.types.ScalaMl.DblVector
import org.scalaml.core.design.Model


		/**
		 * <p>Generic class that defines a model for linear, logistic and regularized regression.</p>
		 * @constructor Create a regression model. [weights] Parameters or weights of the regression. [rss] Residual sum of the squares computed during training.
		 */
case class RegressionModel(val weights: DblVector, val rss: Double) extends Model {
   val persists = "models/regression"
   val size: Int = weights.size
}

// ------------------------  EOF ----------------------------------------------------
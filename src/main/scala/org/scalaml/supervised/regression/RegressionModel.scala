/**
 * Copyright (c) 2013-2015  Patrick Nicolas - Scala for Machine Learning - All rights reserved
 *
 * The source code in this file is provided by the author for the sole purpose of illustrating the 
 * concepts and algorithms presented in "Scala for Machine Learning". It should not be used to 
 * build commercial applications. 
 * ISBN: 978-1-783355-874-2 Packt Publishing.
 * Unless required by applicable law or agreed to in writing, software is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * 
 * Version 0.96d
 */
package org.scalaml.supervised.regression


import org.scalaml.core.Types.ScalaMl.DblVector
import org.scalaml.core.Design.Model


		/**
		 * <p>Generic class that defines a model for linear, logistic and regularized regression.<br>
		 * This class persists the model parameters into file.</p>
		 * @param weights Weights or parameters of the regression computed during the training 
		 * of the model (class instantiation)<br>
		 * @param rss Residual sum of the squares computed during training</p>
		 * @constructor Create a (linear or logistic) regression model. 
		 * @see org.scalaml.core.Design.Model
		 * 
		 * @author Patrick Nicolas
		 * @since January 09, 2014
		 * @note Scala for Machine learning Chapter 6 Regression and regularization.
		 */
case class RegressionModel(val weights: DblVector, val rss: Double) extends Model {
		/**
		 * Constructor that load the model from file "model/RegressionModel"
		 * @param className name of the class of the model, which is also the name of the file
		 * @return instance of the CrfModel with appropriate weights if the model has been 
		 * saved into file
		 * @note The client code that instantiate the model is responsible for catching the 
		 * exception if the file does not exist.
		 */
	def this(className: String) = 
			this({Model.read(className).map( _.split(",").map(_.toDouble)).getOrElse(Array.empty)},0.0)
	
		/**
		 * Write the content of this model (weights) into a file
		 * @return true if the model/weights were saved into file, false otherwise.
		 */
	override def >> : Boolean = write(weights.mkString(","))

		/**
		 * Return the number of weights or regression parameters in this model
		 * @return Size of the regression weight vector is weights are defined, 0 otherwise
		 */
	final val size: Int = if( !weights.isEmpty) weights.size else 0
}

// ------------------------  EOF ----------------------------------------------------
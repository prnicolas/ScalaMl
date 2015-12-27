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
package org.scalaml.supervised.regression


import org.scalaml.core.Types.ScalaMl.{DblVector, DblArray}
import org.scalaml.core.Design.Model


		/**
		 * Generic class that defines a model for linear, logistic and regularized regression.
		 * 
		 * This class persists the model parameters into file.
		 * @param weights Weights or parameters of the regression computed during the training 
		 * of the model (class instantiation)
		 * @param rss Residual sum of the squares computed during training
		 * @see org.scalaml.core.Design.Model
		 * 
		 * @author Patrick Nicolas
		 * @since 0.98 January 09, 2014
		 * @version 0.99.1
		 * @see Scala for Machine learning Chapter 6 "Regression and regularization".
		 */
case class RegressionModel(weights: DblArray, rss: Double) extends Model {
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
	final val size: Int = weights.length
}

// ------------------------  EOF ----------------------------------------------------